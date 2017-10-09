(ns pancake.fixed-width
  (:require [clojure.string :as str]
            [pancake.format :as format]))

(defn ^:private data-error [record data-error]
  (update record :data-errors (comp vec #(conj % data-error))))

(defn parse-line [format index line]
  (letfn [(assoc-field [record field]
            (let [{:keys [id start end]} field
                  length (count line)]
              (if (> end length)
                (-> (assoc record id nil)
                    (data-error {:pred "contains?" :in id}))
                (assoc record id (subs line (dec start) end)))))]
    (let [{:keys [fields length]} format
          line-length (count line)
          record {:data-index index :data-line line}]
      (if (and length (not= line-length length))
        (data-error record {:pred "length-matches?" :in :data-line})
        (reduce assoc-field record fields)))))

(defn ^:private parse-with-format
  [format data]
  (map-indexed (partial parse-line format) data))

(defn ^:private parser [format]
  (map-indexed (partial parse-line format)))

(defn parse
  ([format]
   (format/validate-fixed-width! format)
   (parser format))
  ([format data]
   (format/validate-fixed-width! format)
   (parse-with-format format data)))

(defn parse-str
  [format data]
  (format/validate-fixed-width! format)
  (parse format (str/split-lines data)))
