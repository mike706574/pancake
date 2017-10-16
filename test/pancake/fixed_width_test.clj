(ns pancake.fixed-width-test
  (:refer-clojure :exclude [format])
  (:require [clojure.test :refer [are deftest is]]
            [clojure.string :as str]
            [pancake.core :as pancake]

            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.test.alpha :as stest]))

(stest/instrument)

(deftest parse-xf
  (let [format {:id "test-format"
                :type "fixed-width"
                :description "Test format."
                :fields [{:id :id :start 1 :end 3}
                         {:id :amount :start 4 :end 6}]}
        data ["AAA015"]
        [line] data]
    (is (= [{:data-index 0 :data-line line  :id "AAA" :amount "015"}]
           (into [] (pancake/parse format) data)))))

(deftest parsing-string
  (let [format {:id "test-format"
                :type "fixed-width"
                :description "Test format."
                :fields [{:id :id :start 1 :end 3}
                         {:id :amount :start 4 :end 6}]}]
    (is (= [{:data-index 0 :data-line "AAA015" :id "AAA" :amount "015"}]
           (pancake/parse-str format "AAA015")))))

(deftest no-specified-record-length
  (let [format {:id "test-format"
                :type "fixed-width"
                :description "Test format."
                :fields [{:id :id :start 1 :end 3}
                         {:id :amount :start 4 :end 6}]}]
    (is (= [{:data-index 0
             :data-line "AAA"
             :id "AAA"
             :amount nil
             :data-errors [{:in [:amount] :pred `contains?}]}]
           (pancake/parse format ["AAA"])))

    (is (= [{:data-index 0 :data-line "AAA015" :id "AAA" :amount "015"}]
           (pancake/parse format ["AAA015"])))

    (is (= [{:data-index 0 :data-line "AAA015Z" :id "AAA" :amount "015"}]
           (pancake/parse format ["AAA015Z"])))

    (is (= [{:data-index 0 :data-line "BBB139" :id "BBB" :amount "139"}
            {:data-index 1 :data-line "CCC264" :id "CCC" :amount "264"}]
           (pancake/parse format ["BBB139" "CCC264"])))

    (is (= [{:data-index 0 :data-line "BBB139"  :id "BBB" :amount "139"}
            {:data-index 1 :data-line "CCC264eklawejla" :id "CCC" :amount "264"}]
           (pancake/parse format ["BBB139" "CCC264eklawejla"])))))

(deftest specified-record-length
  (let [format {:id "test-format"
                :description "Test format."
                :type "fixed-width"
                :record-length 6
                :fields [{:id :id :start 1 :end 3}
                         {:id :amount :start 4 :end 6}]}]
    (is (= [{:data-index 0 :data-line "AAA015" :id "AAA" :amount "015"}]
           (pancake/parse format ["AAA015"])))

    (is (=  [{:data-index 0
              :data-line "BBB139Z"
              :data-errors [{:in [:data-line]
                             :pred '(pancake.fixed-width/record-length-is? 6)
                             :val "BBB139Z"}]}]
            (pancake/parse format ["BBB139Z"])))))
