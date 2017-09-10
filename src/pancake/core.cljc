(ns pancake.core
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn max-field-length [format]
  (->> format
       :fields
       (map :end)
       (apply max)))

(defn ^:private parse-line [format index line]
  (letfn [(extract-field [field]
            (let [{:keys [start end]} field]
              (subs line (dec start) end)))
          (assoc-field [record field]
            (assoc record (:id field) (extract-field field)))]
    (let [{:keys [fields length min-length]} format
          line-length (count line)]
      (let [error-category (cond
                             (and length (not= line-length length)) :length-mismatch
                             (< line-length min-length) :too-short)]
        (or (when error-category
              {:data-index index :format-error {:category error-category :data line}})
            (reduce assoc-field {:data-index index} fields) :status :ok)))))

(defn ^:private illegal-format-length
  [{:keys [length min-length]}]
  (when (and length (< length min-length))
    {:category :invalid-format-length}))

(def reserved-field-ids #{:data-index
                          :format-error
                          :format-errors
                          :data-error
                          :data-errors})

(defn ^:private reserved-field
  [format]
  (let [field-ids (set (map (comp keyword :id) (:fields format)))
        used-reserved-fields (set/intersection field-ids reserved-field-ids)]
    (when-not (empty? used-reserved-fields)
      {:category :fields-reserved
       :used-reserved-fields used-reserved-fields})))

(defn ^:private format-errors
  [format]
  (->> [reserved-field illegal-format-length]
       (map #(% format))
       (filter identity)))

(defn ^:private assoc-min-length
  [format]
  (assoc format :min-length (max-field-length format)))

(defn validate-format [format]
  (let [format (assoc-min-length format)
        format-errors (format-errors format)]
    (if (empty? format-errors)
      {:valid? true :format format}
      {:valid? false :format format :format-errors (vec format-errors)})))

(defn ^:private validate-format! [format]
  (let [{:keys [valid? format format-errors] :as response} (validate-format format)]
    (if valid?
      format
      (throw (ex-info "Invalid format." {:format format
                                         :format-errors format-errors})))))

(defn ^:private parse-with-format
  [format data]
  (map-indexed (partial parse-line format) data))

(defn ^:private parser [format]
  (map-indexed (partial parse-line format)))

(defn parse
  ([format]
   (parser (validate-format! format)))
  ([format data]
   (parse-with-format (validate-format! format) data)))

(defn parse-str
  [format data]
  (parse format (str/split-lines data)))

(defn validate-and-parse [format data]
  (let [{:keys [valid? format format-errors]} (validate-format format)]
    (if valid?
      {:status :parsed :format format :data (parse-with-format format data)}
      {:status :invalid-format :format format :format-errors format-errors})))
