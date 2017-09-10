(ns pancake.core-test
  (:refer-clojure :exclude [format])
  (:require [clojure.test :refer [are deftest is]]
            [pancake.core :as core]))

(deftest str-parser
  (let [format {:id "test-format"
                :description "Test format."
                :fields [{:id :id :start 1 :end 3}
                         {:id :amount :start 4 :end 6}]}
        format-with-min-length (assoc format :min-length 6)
        parse (core/str-parser format)]
    (is (= [{:data-index 1 :id "AAA" :amount "015"}]
           (parse "AAA015")))))

(deftest parser
  (let [format {:id "test-format"
                :description "Test format."
                :fields [{:id :id :start 1 :end 3}
                         {:id :amount :start 4 :end 6}]}
        format-with-min-length (assoc format :min-length 6)
        parse (core/parser format)]
    (is (= [{:data-index 1 :id "AAA" :amount "015"}]
           (parse (java.io.StringReader. "AAA015"))))))

(deftest no-specified-length
  (let [format {:id "test-format"
                :description "Test format."
                :fields [{:id :id :start 1 :end 3}
                         {:id :amount :start 4 :end 6}]}
        format-with-min-length (assoc format :min-length 6)
        parse (partial core/parse-str format)]
    (is (= {:status :parsed
            :format format-with-min-length
            :data [{:data-index 1 :id "AAA" :amount "015"}]}
           (parse "AAA015")))

    (is (= {:status :parsed
            :format format-with-min-length
            :data [{:data-index 1 :id "AAA" :amount "015"}]}
           (parse "AAA015Z")))

    (is (= {:status :parsed
            :format format-with-min-length
            :data [{:data-index 1 :id "BBB" :amount "139"}
                   {:data-index 2 :id "CCC" :amount "264"}]}
           (parse "BBB139\nCCC264\n")) )

    (is (= {:status :parsed
            :format format-with-min-length
            :data [{:data-index 1 :id "BBB" :amount "139"}
                   {:data-index 2 :id "CCC" :amount "264"}]}
           (parse "BBB139\nCCC264")))

    (is (= {:status :parsed
            :format format-with-min-length
            :data [{:data-index 1 :id "BBB" :amount "139"}
                   {:data-index 2 :id "CCC" :amount "264"}]}
           (parse "BBB139\nCCC264eklawejla")))))

(deftest specified-length
  (let [format {:id "test-format"
                :description "Test format."
                :length 6
                :fields [{:data-index 1 :id :id :start 1 :end 3}
                         {:data-index 2 :id :amount :start 4 :end 6}]}
        format-with-min-length (assoc format :min-length 6)
        parse (partial core/parse-str format)]

    (is (= {:status :parsed
            :format format-with-min-length
            :data [{:data-index 1 :id "AAA" :amount "015"}]}
           (parse "AAA015")))

    (is (= {:status :parsed
            :format format-with-min-length
            :data [{:data-index 1 :data-error {:category :length-mismatch :data "BBB139Z"}}]}
           (parse "BBB139Z")))))

(deftest invalid-format-length
  (let [format {:id "test-format"
                :description "Test format."
                :length 1
                :fields [{:id :id :start 1 :end 2}]}
        format-with-min-length (assoc format :min-length 2)
        parse (partial core/parse-str format)]
    (is (= {:status :invalid-format
            :format format-with-min-length
            :format-errors [{:category :invalid-format-length}]}
           (parse "")))))

(deftest data-error-is-reserved
  (let [format {:id "test-format"
                :description "Test format."
                :fields [{:id "data-error" :start 1 :end 2}]}
        format-with-min-length (assoc format :min-length 2)
        parse (partial core/parse-str format)]
    (is (= {:status :invalid-format
            :format format-with-min-length
            :format-errors
            [{:category :fields-reserved
              :used-reserved-fields #{:data-error}}]}
           (parse "")))))

(deftest data-index-is-reserved
  (let [format {:id "test-format"
                :description "Test format."
                :fields [{:id "data-index" :start 1 :end 2}]}
        format-with-min-length (assoc format :min-length 2)
        parse (partial core/parse-str format)]
    (is (= {:status :invalid-format
            :format format-with-min-length
            :format-errors
            [{:category :fields-reserved
              :used-reserved-fields #{:data-index}}]}
           (parse "")))))

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
           (parse "")))))
