(ns pancake.core
  (:require [clojure.spec.alpha :as s]
            [pancake.delimited :as delimited]
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
   ((parse-fn format) format))
  ([format data]
   ((parse-fn format) format data)))

(s/def :pancake/lines (s/coll-of string?))

(s/fdef parse
  :args (s/or :xform (s/cat :format :pancake/format)
              :data (s/cat :format :pancake/format :data :pancake/lines))
  :ret (s/or :xform fn?
             :data (s/coll-of map?)))

(defn ^:private parse-str-fn [format]
  (case (:type format)
      "delimited" delimited/parse-str
      "fixed-width" fixed-width/parse-str
      (unsupported-format format)))

(defn parse-str
  [format data]
  ((parse-str-fn format) format data))

(s/fdef parse-str
  :args (s/cat :format :pancake/format :data string?)
  :ret (s/coll-of map?))
