(ns pancake.core
  (:require [pancake.delimited :as delimited]
            [pancake.fixed-width :as fixed-width]))

(defn ^:private unsupported-format [format]
  (throw (ex-info (str "Unsupported format type: " (:type format)) format)))

(defn ^:private parse-fn [format]
  (case (:type format)
      "delimited" delimited/parse
      "fixed-width" fixed-width/parse
      (unsupported-format format)))

(defn parse
  ([format]
   ((parse-fn) format))
  ([format data]
   ((parse-fn) format data)))

(defn ^:private parse-str-fn [format]
  (case (:type format)
      "delimited" delimited/parse-str
      "fixed-width" fixed-width/parse-str
      (unsupported-format format)))

(defn parse-str
  [format data]
  ((parse-str-fn format) format data))
