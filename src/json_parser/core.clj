(ns json-parser.core
  (:require [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.java.io :as io])
  (:import [java.io BufferedReader StringReader]))

(def number-regexp #"-?\d+(\.\d+(e-?\d+)?)?")

(s/def :parser/string string?)
(s/def :parser/num-string
  (s/and :parser/string
         #(re-matches number-regexp %)))

(s/def :parser/exists #(.exists (io/as-file %)))

(s/def :parser/number number?)
(s/def :parser/boolean boolean?)
(s/def :parser/keyword keyword?)
(s/def :parser/char char?)

(s/def :parser/value
  (s/or :object :parser/object
        :array :parser/array
        :number :parser/number
        :boolean :parser/boolean
        :string :parser/string))

(s/def :parser/attribute
  (s/map-of :parser/keyword :parser/value
            :distinct true :count 1))

(s/def :parser/array
  (s/coll-of :parser/value))

(s/def :parser/object
  (s/map-of :parser/keyword :parser/value
            :distinct true))

(s/def :parser/json
  (s/or :object :parser/object
        :array :parser/array))

;; TODO: implement macro to automatically insert call to consume-ws
(defmacro ignore-ws-let
  [reader])

(declare read-char)

(defn- read-char
  "Reads a character from `reader`."
  [reader]
  {:post [(s/valid? :parser/char %)]}
  (char (.read reader)))

(defn- matches-chars?
  "Attempts to read the characters in `expected` from `reader`."
  [reader expected]
  {:post [(s/valid? :parser/boolean %)]}
  (let [_ (.mark reader 1)
        actual (repeatedly (count expected) #(read-char reader))]
    (if (= actual expected)
      true
      (let [_ (.reset reader)]
        false))))

(defn- consume
  "Continues reading characters from `reader` until terminating character `end`
  is found. Returns the characters read as a string."
  [reader end]
  {:post [(s/valid? :parser/string %)]}
  (loop [c (read-char reader)
         res ""]
    (cond
      (= c end) res
      ;; append the character after the backslash
      ;; to the result before reading another character
      (= c \\) (let [c (read-char reader)]
                 (recur (read-char reader)
                        (str res c)))
      :else (recur (read-char reader)
                   (str res c)))))

(defn- consume-ws
  "Continues reading whitespace characters from `reader` until a non-whitespace
  character is encountered."
  [reader]
  (loop [c ""]
    (let [_ (.mark reader 1)
          s (str (read-char reader))]
      (if (str/blank? s)
        (recur (str c s))
        (let [_  (.reset reader)]
          c)))))

(defn- valid-number-section?
  [num sep section]
  (if (str/includes? num sep)
    (if-some [_ section]
      true
      (throw (Exception.
              (str "Number " num " invalid. It includes separator " sep " but no valid section."))))
    true))

(defn- consume-digits
  "Continues reading characters from `reader` as long as the current character
  is a valid digit character. Returns the characters read as a string."
  [reader]
  (loop [_ (.mark reader 1)
         digit (str (read-char reader))
         res ""]
    (if (re-find #"-|\d|\.|e" digit)
      (recur (.mark reader 1) (str (read-char reader))
             (str res digit))
      (let [_ (.reset reader)]
        (if-some [[_num decimal exp] (re-find number-regexp res)]
          (do
            (doall
             (map #(apply valid-number-section? %)
                  [[res "." decimal] [res "e" exp]]))
            res)
          (throw (Exception. (str "Not a valid number " res "."))))))))

(declare read-array)
(declare read-object)

(defn- read-str
  "Consume characters from `reader` until string end."
  [reader]
  {:post [(s/valid? :parser/string %)]}
  (consume reader \"))

(defn- read-number
  "Converts the digits consumed from `reader` into a number."
  [reader]
  {:post [(s/valid? :parser/number %)]}
  (when-some [digits (consume-digits reader)]
    (Double/parseDouble digits)))

(defn- read-val
  "Parses a value in a JSON attribute. Returns a Clojure representation
  of the value."
  [reader]
  {:post [(s/valid? :parser/value %)]}
  ;; create a binding so we keep the side effect of reader.mark().
  ;; we are doing this because the call to read-char will remove
  ;; the next character from the BufferedReader, but we need that
  ;; character if it is a digit, because we need to parse any other
  ;; digits that come after it. marking our position here saves us
  ;; from having to pass the character into read-digit, allowing it
  ;; to have the same standard arguments as the rest of the read
  ;; functions.
  (let [_ (.mark reader 1)
        c (read-char reader)]
    (cond
      (= c \{) (read-object reader)
      (= c \[) (read-array reader)
      (re-find #"-|\d" (str c)) (let [_ (.reset reader)]
                                  (read-number reader))
      (= c \") (read-str reader)
      (= c \t) (when (matches-chars? reader [\r \u \e])
                 true)
      (= c \f) (when (matches-chars? reader [\a \l \s \e])
                 false)
      (= c \n) (when (matches-chars? reader [\u \l \l])
                 nil))))

(defn- read-attribute
  "Parses a single JSON object attribute, and returns a key/value pair."
  [reader]
  {:post [(s/valid? :parser/attribute %)]}
  (let [_ (consume-ws reader)
        key (keyword (read-str reader))]
    (if (matches-chars? reader [\:])
      (let [_ (consume-ws reader)
            val (read-val reader)]
        (assoc {} key val))
      (throw (Exception. (str "No value found after key " key "."))))))

(defn- read-array
  "Parses the contents of a JSON array, and builds a vector from it.
  Once a ] is found, parsing ends and the vector is returned."
  [reader]
  {:post [(s/valid? :parser/array %)]}
  (loop [arr []]
    (let [_ (consume-ws reader)
          _ (.mark reader 1)]
      (if (= (read-char reader) \])
        arr
        (let [_ (.reset reader)
              arr (conj arr (read-val reader))
              _ (consume-ws reader)]
          (case (read-char reader)
            \, (recur arr)
            \] arr
            (throw (Exception. (str "No comma or left bracket found after array value.")))))))))

(defn- read-object
  "Parses the attributes of a JSON object and builds a map from the
  contents. Once a } is found, parsing ends and the map is returned."
  [reader]
  {:post [(s/valid? :parser/object %)]}
  (loop [obj {}]
    (let [_ (consume-ws reader)]
      (case (read-char reader)
        \" (let [obj (merge obj (read-attribute reader))
                 _ (consume-ws reader)]
             (case (read-char reader)
               \, (recur obj)
               \} obj
               (throw (Exception. (str "No comma or left brace found after object field.")))))
        \} obj))))

(defn- read-json
  "Checks the first character in `reader` and begins parsing
  based on what it finds. If { is found, then the returned
  object will be a map. If a [ is found, then it will be a
  vector."
  [reader]
  {:post [(s/valid? :parser/json %)]}
  (let [_ (consume-ws reader)
        char (read-char reader)]
    (cond
      (= char \[) (read-array reader)
      (= char \{) (read-object reader)
      :else
      (throw (Exception. (str "Not valid JSON object. Encountered: " char))))))

(defn parse-json-string
  "Decodes JSON string `source` to a Clojure representation.
  Points a BufferedReader to `source` to parse the string."
  [source]
  {:pre [(s/valid? :parser/string source)]}
  (with-open [source (BufferedReader. (StringReader. source))]
    (read-json source)))

(defn parse-json-file
  "Loads a JSON string from `file` and parses the string to a
  Clojure representation."
  [file]
  {:pre [(s/valid? :parser/exists file)]}
  (parse-json-string (str/trim (slurp file))))
