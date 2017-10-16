(ns pancake.delimited-test
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
                :type "delimited"
                :delimiter \|
                :description "Test format."
                :cells [{:id :id :index 0}
                        {:id :amount :index 1}]}
        data ["AAA|015"]
        [line] data]
    (is (= [{:data-index 0 :data-line line  :id "AAA" :amount "015"}]
           (into [] (pancake/parse format) data)))))

(deftest parse-str
  (let [format {:id "test-format"
                :type "delimited"
                :delimiter \|
                :description "Test format."
                :cells [{:id :id :index 0}
                        {:id :amount :index 1}]}]
    (is (= [{:data-index 0 :data-line "AAA|015" :id "AAA" :amount "015"}]
           (pancake/parse-str format "AAA|015")))))

(deftest no-specified-cell-count
  (let [format {:id "test-format"
                :type "delimited"
                :delimiter \|
                :description "Test format."
                :cells [{:id :id :index 0}
                        {:id :amount :index 1}]}]
    (is (= [{:data-index 0
             :data-line "AAA"
             :id "AAA"
             :amount nil
             :data-errors [{:in [:amount] :pred `contains?}]}]
           (pancake/parse format ["AAA"])))

    (is (= [{:data-index 0 :data-line "AAA|015" :id "AAA" :amount "015"}]
           (pancake/parse format ["AAA|015"])))

    (is (= [{:data-index 0 :data-line "AAA|015|Z" :id "AAA" :amount "015"}]
           (pancake/parse format ["AAA|015|Z"])))

    (is (= [{:data-index 0 :data-line "BBB|139" :id "BBB" :amount "139"}
            {:data-index 1 :data-line "CCC|264" :id "CCC" :amount "264"}]
           (pancake/parse format ["BBB|139" "CCC|264"])))

    (is (= [{:data-index 0 :data-line "BBB|139"  :id "BBB" :amount "139"}
            {:data-index 1 :data-line "CCC|264|eklawejla" :id "CCC" :amount "264"}]
           (pancake/parse format ["BBB|139" "CCC|264|eklawejla"])))))

(deftest specified-cell-count
  (let [format {:id "test-format"
                :description "Test format."
                :type "delimited"
                :delimiter "|"
                :cell-count 2
                :cells [{:id :id :index 0}
                        {:id :amount :index 1}]}]
    (is (= [{:data-index 0 :data-line "AAA|015" :id "AAA" :amount "015"}]
           (pancake/parse format ["AAA|015"])))

    (is (=  [{:data-index 0
              :data-line "BBB|139|Z"
              :data-errors [{:in [:data-cell]
                             :pred `(pancake.delimited/cell-count-is? 2)
                             :val ["BBB" "139" "Z"]}]}]
           (pancake/parse format ["BBB|139|Z"])))))

(deftest csv
  (let [format {:id "test-format"
                :type "delimited"
                :delimiter ","
                :description "Test format."
                :cells [{:id :id :index 0}
                        {:id :amount :index 1}]}]
    (is (= [{:data-index 0 :data-line "AAA,015" :id "AAA" :amount "015"}]
           (pancake/parse format ["AAA,015"])))))
