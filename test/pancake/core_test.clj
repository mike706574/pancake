(ns pancake.core-test
  (:refer-clojure :exclude [format])
  (:require [clojure.test :refer [are deftest is]]
            [pancake.core :as core]))

(deftest no-specified-length
  (let [format {:id "test-format"
                :description "Test format."
                :fields [{:id "id" :start 1 :end 3}
                         {:id "amount" :start 4 :end 6}]}
        format-with-min-length (assoc format :min-length 6)
        parse (partial core/parse-str format)]
    (is (= {:status :parsed
            :format format-with-min-length
            :data [{:id "AAA" :amount "015"}]}
           (parse "AAA015")))

    (is (= {:status :parsed
            :format format-with-min-length
            :data [{:id "AAA" :amount "015"}]}
           (parse "AAA015Z")))

    (is (= {:status :parsed
            :format format-with-min-length
            :data [{:id "BBB" :amount "139"}
                   {:id "CCC" :amount "264"}]}
           (parse "BBB139\nCCC264\n")) )

    (is (= {:status :parsed
            :format format-with-min-length
            :data [{:id "BBB" :amount "139"}
                   {:id "CCC" :amount "264"}]}
           (parse "BBB139\nCCC264")))

    (is (= {:status :parsed
            :format format-with-min-length
            :data [{:id "BBB" :amount "139"}
                   {:id "CCC" :amount "264"}]}
           (parse "BBB139\nCCC264eklawejla")))))

(deftest specified-length
  (let [format {:id "test-format"
                :description "Test format."
                :length 6
                :fields [{:id "id" :start 1 :end 3}
                         {:id "amount" :start 4 :end 6}]}
        format-with-min-length (assoc format :min-length 6)
        parse (partial core/parse-str format)]

    (is (= {:status :parsed
            :format format-with-min-length
            :data [{:id "AAA" :amount "015"}]}
           (parse "AAA015")))

    (is (= {:status :parsed
            :format
            {:id "test-format"
             :description "Test format."
             :length 6
             :fields
             [{:id "id" :start 1 :end 3} {:id "amount" :start 4 :end 6}]
             :min-length 6}
            :data
            [{:data-error {:category :length-mismatch :data "BBB139Z"}}]}
           (parse "BBB139Z")))))

(deftest invalid-format-length
  (let [format {:id "test-format"
                :description "Test format."
                :length 1
                :fields [{:id "id" :start 1 :end 2}]}
        format-with-min-length (assoc format :min-length 2)
        parse (partial core/parse-str format)]
    (is (= {:status :invalid-format
            :format format-with-min-length
            :format-errors [:invalid-format-length]}
           (parse "")))))

(deftest reserved-field
  (let [format {:id "test-format"
                :description "Test format."
                :fields [{:id "data-error" :start 1 :end 2}]}
        format-with-min-length (assoc format :min-length 2)
        parse (partial core/parse-str format)]
    (is (= {:status :invalid-format
            :format format-with-min-length
            :format-errors [:field-reserved]}
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
            :format-errors [:field-reserved :invalid-format-length]}
           (parse "")))))
