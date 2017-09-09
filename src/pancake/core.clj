(ns pancake.core
  (:require [clojure.java.io :as io]))

(defn ^:private  max-field-length [format]
  (->> format
       :fields
       (map :end)
       (apply max)))

(defn ^:private  parse-line [format line]
  (letfn [(extract-field [field]
            (let [{:keys [start end]} field]
              (subs line (dec start) end)))
          (assoc-field [record field]
            (assoc record (keyword (:id field)) (extract-field field)))]
    (let [{:keys [fields length min-length]} format
          line-length (count line)]
      (let [error-category (cond
                             (and length (not= line-length length)) :length-mismatch
                             (< line-length min-length) :too-short)]
        (or (when error-category
              {:data-error {:category error-category :data line}})
            (reduce assoc-field {} fields) :status :ok)))))

(defn ^:private illegal-format-length
  [{:keys [length min-length]}]
  (when (and length (< length min-length))
    :invalid-format-length))

(defn ^:private reserved-field
  [format]
  (let [field-ids (map (comp keyword :id) (:fields format))]
    (when (contains? (set field-ids) :data-error)
      :field-reserved)))

(defn ^:private format-errors
  [format]
  (->> [reserved-field illegal-format-length]
       (map #(% format))
       (filter identity)))

(defn parse [format x]
  (let [min-length (max-field-length format)
        format (assoc format :min-length min-length)
        format-errors (format-errors format)]
    (if (empty? format-errors)
      (with-open [reader (io/reader x)]
        {:status :parsed
         :format format
         :data (doall (mapv (partial parse-line format) (line-seq reader)))})
      {:status :invalid-format
       :format format
       :format-errors format-errors})))

(defn parse-str [format str]
  (parse format (java.io.StringReader. str)))
