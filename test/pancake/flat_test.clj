(ns pancake.flat-test
  (:refer-clojure :exclude [format])
  (:require [clojure.test :refer [are deftest is]]
            [clojure.string :as str]
            [pancake.flat :as flat]))

(deftest parse-xf
  (let [format {:id "test-format"
                :type "flat"
                :description "Test format."
                :fields [{:id :id :start 1 :end 3}
                         {:id :amount :start 4 :end 6}]}
        data ["AAA015"]
        [line] data]
    (is (= [{:data-index 0 :data-line line  :id "AAA" :amount "015"}]
           (into [] (flat/parse format) data)))))

(deftest parse-str
  (let [format {:id "test-format"
                :type "flat"
                :description "Test format."
                :fields [{:id :id :start 1 :end 3}
                         {:id :amount :start 4 :end 6}]}]
    (is (= [{:data-index 0 :data-line "AAA015" :id "AAA" :amount "015"}]
           (flat/parse-str format "AAA015")))))

(= {:in :amount, :pred "contains?"}
   {:pred "contains?", :in :amount})
(deftest no-specified-length
  (let [format {:id "test-format"
                :type "flat"
                :description "Test format."
                :fields [{:id :id :start 1 :end 3}
                         {:id :amount :start 4 :end 6}]}]
    (is (= [{:data-index 0
             :data-line "AAA"
             :id "AAA"
             :amount nil
             :data-errors [{:in :amount :pred "contains?"}]}]
           (flat/parse format ["AAA"])))

    (is (= [{:data-index 0 :data-line "AAA015" :id "AAA" :amount "015"}]
           (flat/parse format ["AAA015"])))

    (is (= [{:data-index 0 :data-line "AAA015Z" :id "AAA" :amount "015"}]
           (flat/parse format ["AAA015Z"])))

    (is (= [{:data-index 0 :data-line "BBB139" :id "BBB" :amount "139"}
            {:data-index 1 :data-line "CCC264" :id "CCC" :amount "264"}]
           (flat/parse format ["BBB139" "CCC264"])))

    (is (= [{:data-index 0 :data-line "BBB139"  :id "BBB" :amount "139"}
            {:data-index 1 :data-line "CCC264eklawejla" :id "CCC" :amount "264"}]
           (flat/parse format ["BBB139" "CCC264eklawejla"])))))

(deftest specified-length
  (let [format {:id "test-format"
                :description "Test format."
                :type "flat"
                :length 6
                :fields [{:id :id :start 1 :end 3}
                         {:id :amount :start 4 :end 6}]}]
    (is (= [{:data-index 0 :data-line "AAA015" :id "AAA" :amount "015"}]
           (flat/parse format ["AAA015"])))

    (is (=  [{:data-index 0
              :data-line "BBB139Z"
              :data-errors [{:in :data-line :pred "length-matches?"}]}]
           (flat/parse format ["BBB139Z"])))))
