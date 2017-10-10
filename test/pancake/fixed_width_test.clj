(ns pancake.fixed-width
  (:refer-clojure :exclude [format])
  (:require [clojure.test :refer [are deftest is]]
            [clojure.string :as str]
            [pancake.fixed-width :as fixed-width]))

(deftest parse-xf
  (let [format {:id "test-format"
                :type "fixed-width"
                :description "Test format."
                :fields [{:id :id :start 1 :end 3}
                         {:id :amount :start 4 :end 6}]}
        data ["AAA015"]
        [line] data]
    (is (= [{:data-index 0 :data-line line  :id "AAA" :amount "015"}]
           (into [] (fixed-width/parse format) data)))))

(deftest parsing-string
  (let [format {:id "test-format"
                :type "fixed-width"
                :description "Test format."
                :fields [{:id :id :start 1 :end 3}
                         {:id :amount :start 4 :end 6}]}]
    (is (= [{:data-index 0 :data-line "AAA015" :id "AAA" :amount "015"}]
           (fixed-width/parse-str format "AAA015")))))

(deftest no-specified-length
  (let [format {:id "test-format"
                :type "fixed-width"
                :description "Test format."
                :fields [{:id :id :start 1 :end 3}
                         {:id :amount :start 4 :end 6}]}]
    (is (= [{:data-index 0
             :data-line "AAA"
             :id "AAA"
             :amount nil
             :data-errors [{:key :amount :pred "contains?"}]}]
           (fixed-width/parse format ["AAA"])))

    (is (= [{:data-index 0 :data-line "AAA015" :id "AAA" :amount "015"}]
           (fixed-width/parse format ["AAA015"])))

    (is (= [{:data-index 0 :data-line "AAA015Z" :id "AAA" :amount "015"}]
           (fixed-width/parse format ["AAA015Z"])))

    (is (= [{:data-index 0 :data-line "BBB139" :id "BBB" :amount "139"}
            {:data-index 1 :data-line "CCC264" :id "CCC" :amount "264"}]
           (fixed-width/parse format ["BBB139" "CCC264"])))

    (is (= [{:data-index 0 :data-line "BBB139"  :id "BBB" :amount "139"}
            {:data-index 1 :data-line "CCC264eklawejla" :id "CCC" :amount "264"}]
           (fixed-width/parse format ["BBB139" "CCC264eklawejla"])))))

(deftest specified-length
  (let [format {:id "test-format"
                :description "Test format."
                :type "fixed-width"
                :length 6
                :fields [{:id :id :start 1 :end 3}
                         {:id :amount :start 4 :end 6}]}]
    (is (= [{:data-index 0 :data-line "AAA015" :id "AAA" :amount "015"}]
           (fixed-width/parse format ["AAA015"])))

    (is (=  [{:data-index 0
              :data-line "BBB139Z"
              :data-errors [{:key :data-line :pred "length-matches?"}]}]
           (fixed-width/parse format ["BBB139Z"])))))
