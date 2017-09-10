(ns pancake.core-test
  (:refer-clojure :exclude [format])
  (:require [clojure.test :refer [are deftest is]]
            [clojure.string :as str]
            [pancake.core :as core]))

(deftest parse-xf
  (let [format {:id "test-format"
                :description "Test format."
                :fields [{:id :id :start 1 :end 3}
                         {:id :amount :start 4 :end 6}]}
        format-with-min-length (assoc format :min-length 6)]
    (is (= [{:data-index 0 :id "AAA" :amount "015"}]
           (into [] (core/parse format) ["AAA015"])))))

(deftest parse-str
  (let [format {:id "test-format"
                :description "Test format."
                :fields [{:id :id :start 1 :end 3}
                         {:id :amount :start 4 :end 6}]}]
    (is (= [{:data-index 0 :id "AAA" :amount "015"}]
           (core/parse-str format "AAA015")))))

(deftest no-specified-length
  (let [format {:id "test-format"
                :description "Test format."
                :fields [{:id :id :start 1 :end 3}
                         {:id :amount :start 4 :end 6}]}]
    (is (= [{:data-index 0 :id "AAA" :amount "015"}]
           (core/parse format ["AAA015"])))

    (is (= [{:data-index 0 :id "AAA" :amount "015"}]
           (core/parse format ["AAA015Z"])))

    (is (= [{:data-index 0 :id "BBB" :amount "139"}
            {:data-index 1 :id "CCC" :amount "264"}]
           (core/parse format ["BBB139" "CCC264"])))

    (is (= [{:data-index 0 :id "BBB" :amount "139"}
            {:data-index 1 :id "CCC" :amount "264"}]
           (core/parse format ["BBB139" "CCC264eklawejla"])))))

(deftest specified-length
  (let [format {:id "test-format"
                :description "Test format."
                :length 6
                :fields [{:data-index 0 :id :id :start 1 :end 3}
                         {:data-index 1 :id :amount :start 4 :end 6}]}
        format-with-min-length (assoc format :min-length 6)]
    (is (= {:status :parsed
            :format format-with-min-length
            :data [{:data-index 0 :id "AAA" :amount "015"}]}
           (core/validate-and-parse format ["AAA015"])))

    (is (= {:status :parsed
            :format format-with-min-length
            :data [{:data-index 0 :data-error {:category :length-mismatch :data "BBB139Z"}}]}
           (core/validate-and-parse format ["BBB139Z"])))))

(deftest invalid-format-length
  (let [format {:id "test-format"
                :description "Test format."
                :length 1
                :fields [{:id :id :start 1 :end 2}]}
        format-with-min-length (assoc format :min-length 2)]
    (is (= {:status :invalid-format
            :format format-with-min-length
            :format-errors [{:category :invalid-format-length}]}
           (core/validate-and-parse format [])))))

(deftest data-error-is-reserved
  (let [format {:id "test-format"
                :description "Test format."
                :fields [{:id "data-error" :start 1 :end 2}]}
        format-with-min-length (assoc format :min-length 2)]
    (is (= {:status :invalid-format
            :format format-with-min-length
            :format-errors
            [{:category :fields-reserved
              :used-reserved-fields #{:data-error}}]}
           (core/validate-and-parse format [])))))

(deftest data-index-is-reserved
  (let [format {:id "test-format"
                :description "Test format."
                :fields [{:id "data-index" :start 1 :end 2}]}
        format-with-min-length (assoc format :min-length 2)
        parse (partial core/parse-str format)]
    (is (= {:status :invalid-format
            :format format-with-min-length
            :format-errors [{:category :fields-reserved
                             :used-reserved-fields #{:data-index}}]}
           (core/validate-and-parse format [])))))

(deftest invalid-format-length-and-reserved-field
  (let [format {:id "test-format"
                :description "Test format."
                :length 1
                :fields [{:id "data-error" :start 1 :end 2}]}
        format-with-min-length (assoc format :min-length 2)
        parse (partial core/parse-str format)]
    (is (= {:status :invalid-format
            :format format-with-min-length
            :format-errors [{:category :fields-reserved
                             :used-reserved-fields #{:data-error}}
                            {:category :invalid-format-length}]}
           (core/validate-and-parse format [])))))
