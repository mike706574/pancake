(ns pancake.delimited-test
  (:refer-clojure :exclude [format])
  (:require [clojure.test :refer [are deftest is]]
            [clojure.string :as str]
            [pancake.delimited :as delimited]))

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
           (into [] (delimited/parse format) data)))))

(deftest parse-str
  (let [format {:id "test-format"
                :type "delimited"
                :delimiter\|
                :description "Test format."
                :cells [{:id :id :index 0}
                        {:id :amount :index 1}]}]
    (is (= [{:data-index 0 :data-line "AAA|015" :id "AAA" :amount "015"}]
           (delimited/parse-str format "AAA|015")))))

(deftest no-specified-length
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
             :data-errors [{:key :amount :pred "contains?"}]}]
           (delimited/parse format ["AAA"])))

    (is (= [{:data-index 0 :data-line "AAA|015" :id "AAA" :amount "015"}]
           (delimited/parse format ["AAA|015"])))

    (is (= [{:data-index 0 :data-line "AAA|015|Z" :id "AAA" :amount "015"}]
           (delimited/parse format ["AAA|015|Z"])))

    (is (= [{:data-index 0 :data-line "BBB|139" :id "BBB" :amount "139"}
            {:data-index 1 :data-line "CCC|264" :id "CCC" :amount "264"}]
           (delimited/parse format ["BBB|139" "CCC|264"])))

    (is (= [{:data-index 0 :data-line "BBB|139"  :id "BBB" :amount "139"}
            {:data-index 1 :data-line "CCC|264|eklawejla" :id "CCC" :amount "264"}]
           (delimited/parse format ["BBB|139" "CCC|264|eklawejla"])))))

(deftest specified-length
  (let [format {:id "test-format"
                :description "Test format."
                :type "delimited"
                :delimiter "|"
                :length 2
                :cells [{:id :id :index 0}
                         {:id :amount :index 1}]}]
    (is (= [{:data-index 0 :data-line "AAA|015" :id "AAA" :amount "015"}]
           (delimited/parse format ["AAA|015"])))

    (is (=  [{:data-index 0
              :data-line "BBB|139|Z"
              :data-errors [{:key :data-line
                             :pred "length-matches?"
                             :parsed ["BBB" "139" "Z"]}]}]
           (delimited/parse format ["BBB|139|Z"])))))

(deftest csv
  (let [format {:id "test-format"
                :type "delimited"
                :delimiter ","
                :description "Test format."
                :cells [{:id :id :index 0}
                        {:id :amount :index 1}]}]
    (is (= [{:data-index 0 :data-line "AAA,015" :id "AAA" :amount "015"}]
           (delimited/parse format ["AAA,015"])))))
