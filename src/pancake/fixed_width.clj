(ns pancake.fixed-width
  (:require [clojure.string :as str]
            [pancake.format :as format]))

(defn ^:private data-error [record data-error]
  (update record :data-errors (comp vec #(conj % data-error))))

(defn parse-line [format index line]
  (letfn [(assoc-field [record field]
            (let [{:keys [id start end]} field
                  line-length (count line)]
              (if (> end line-length)
                (-> (assoc record id nil)
                    (data-error {:pred `contains? :in [id]}))
                (assoc record id (subs line (dec start) end)))))]
    (let [{:keys [fields record-length]} format
          line-length (count line)
          record {:data-index index :data-line line}]
      (if (and record-length (not= line-length record-length))
        (data-error record {:pred `(record-length-is? ~record-length)
                            :in [:data-line]
                            :val line})
        (reduce assoc-field record fields)))))

(defn ^:private parse-with-format
  [format data]
  (map-indexed (partial parse-line format) data))

(defn ^:private parser [format]
  (map-indexed (partial parse-line format)))

(defn parse
  ([format]
   (parser format))
  ([format data]
   (parse-with-format format data)))

(defn parse-str
  [format data]
  (parse format (str/split-lines data)))
