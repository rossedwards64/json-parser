(ns json-parser.core-test
  (:require [json-parser.core :as parser]
            [clojure.test :as test :refer [deftest is testing]]))

(def person
  "{
    \"name\": \"Ross\",
    \"age\": 21,
    \"hobbies\": [
      \"programming\"
    ]
   }")

(def person-with-object-str
  "{
    \"name\": \"Ross\",
    \"age\": 21,
    \"hobbies\": [
      \"programming\"
    ],
    \"pc-build\": {
      \"processor\": \"Ryzen 5 5600X\",
      \"graphics-card\": \"RX 5700XT\",
      \"memory\": \"Corsair Vengeance 16GB\",
      \"motherboard\": \"B450 Aorus PRO\",
      \"storage\": \"4TB\"
    }
   }")

(def person-with-object-map
  {:name "Ross" :age 21.0 :hobbies ["programming"]
   :pc-build {:processor "Ryzen 5 5600X"
              :graphics-card "RX 5700XT"
              :memory "Corsair Vengeance 16GB"
              :motherboard "B450 Aorus PRO"
              :storage "4TB"}})

(def decimal-number "{\"number\":21.2345}")
(def scientific-notation "{\"number\":76.435e6}")

(deftest happy-path
  (testing "Valid JSON is parsed correctly"
    (is (= (parser/parse-json-string person)
           {:name "Ross" :age 21.0 :hobbies ["programming"]}))
    (is (= (parser/parse-json-string person-with-object-str)
           person-with-object-map))
    (is (= (parser/parse-json-string decimal-number)
           {:number 21.2345}))
    (is (= (parser/parse-json-string scientific-notation)
           {:number 7.6435E7})))
  (testing "JSON is parsed from a file"
    (let [temp-file (java.io.File/createTempFile "file" "json")]
      (spit temp-file person-with-object-str)
      (is (= (parser/parse-json-file temp-file) person-with-object-map)))))

(def invalid-attribute "{ \"name\" \"Ross\" }")
(def invalid-decimal-number "{\"age\":21.}")
(def invalid-standard-form-number "{\"age\":21e}")

(deftest failure
  (testing "Ill-formed JSON throws the correct exception"
    (is (thrown-with-msg? Exception #"No value found after key"
                          (parser/parse-json-string invalid-attribute)))
    (is (thrown-with-msg? Exception #".*It includes separator \..*"
                          (parser/parse-json-string invalid-decimal-number)))
    (is (thrown-with-msg? Exception #".*It includes separator e.*"
                          (parser/parse-json-string invalid-standard-form-number)))))
