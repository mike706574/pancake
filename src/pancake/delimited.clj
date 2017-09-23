(ns pancake.delimited
  (:require [clojure.string :as str]
            [pancake.format :as format])
  (:import [java.io PushbackReader StringReader]))

(def ^{:private true} end -1)

(defn ^:private read-quoted-cell [^PushbackReader reader ^StringBuilder sb sep quote]
  (loop [ch (.read reader)]
    (condp == ch
      quote (let [next-ch (.read reader)]
              (condp == next-ch
                quote (do (.append sb (char quote))
                          (recur (.read reader)))
                sep :sep
                end :end
                :err))
      end :err
      (do (.append sb (char ch))
          (recur (.read reader))))))

(defn ^:private read-cell [^PushbackReader reader ^StringBuilder sb sep quote]
  (let [first-ch (.read reader)]
    (if (== first-ch quote)
      (read-quoted-cell reader sb sep quote)
      (loop [ch first-ch]
        (condp == ch
          sep :sep
          end :end
          (do (.append sb (char ch))
              (recur (.read reader))))))))

(defn ^:private read-record [reader sep quote]
  (loop [data (transient [])]
    (let [cell (StringBuilder.)
          sentinel (read-cell reader cell sep quote)
          data (conj! data (str cell))]
      (if (= sentinel :sep)
        (recur data)
        [(= sentinel :end) (persistent! data)]))))

(defn ^:private char-int [x]
  (cond
    (int? x) x
    (char? x) (int x)
    (string? x) (int (first x))))

(defn ^:private data-error [record data-error]
  (let [data-errors (:data-errors record)]
    (assoc record :data-errors (vec (conj data-errors data-error)))))

(defn ^:private parse-line [format index line]
  (letfn [(assoc-field [data record field]
            (let [{:keys [id index]} field]
              (if-let [val (get data index)]
                (assoc record id val)
                (-> record
                    (assoc id nil)
                    (data-error [:pred "contains?" :in id])))))]
    (let [{:keys [fields length separator quote]} format
          reader (PushbackReader. (StringReader. line))
          [valid? data] (read-record reader (char-int separator) (char-int quote))
          record {:data-index index :data-line line}]
      (if valid?
        (if (and length (not= (count data) length))
          (data-error record {:pred "length-matches?" :in :data-line :parsed data})
          (reduce (partial assoc-field data) record fields))
        (data-error record {:pred "valid-cell?" :in :data-line :up-to data})))))

(defn ^:private parse-with-format
  [format data]
  (map-indexed (partial parse-line format) data))

(defn ^:private parser [format]
  (map-indexed (partial parse-line format)))

(defn parse
  ([format]
   (format/validate-delimited! format)
   (parser format))
  ([format data]
   (format/validate-delimited! format)
   (parse-with-format format data)))

(defn parse-str
  [format data]
  (format/validate-delimited! format)
  (parse format (str/split-lines data)))
