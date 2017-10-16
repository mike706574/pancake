(ns pancake.delimited
  (:require [clojure.string :as str])
  (:import [java.io PushbackReader StringReader]))

(def ^{:private true} end -1)
(def ^{:private true} quote 34)
(def ^{:private true} quote-char \")

(defn ^:private read-quoted-cell [^PushbackReader reader ^StringBuilder sb sep]
  (loop [ch (.read reader)]
    (condp == ch
      quote (let [next-ch (.read reader)]
              (condp == next-ch
                quote (do (.append sb quote-char)
                          (recur (.read reader)))
                sep :sep
                end :end
                :err))
      end :err
      (do (.append sb (char ch))
          (recur (.read reader))))))

(defn ^:private read-cell [^PushbackReader reader ^StringBuilder sb sep]
  (let [first-ch (.read reader)]
    (if (== first-ch quote)
      (read-quoted-cell reader sb sep quote)
      (loop [ch first-ch]
        (condp == ch
          sep :sep
          end :end
          (do (.append sb (char ch))
              (recur (.read reader))))))))

(defn ^:private read-record [reader sep]
  (loop [data (transient [])]
    (let [cell (StringBuilder.)
          sentinel (read-cell reader cell sep)
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
  (letfn [(assoc-cell [data record cell]
            (let [{:keys [id index]} cell]
              (if-let [val (get data index)]
                (assoc record id val)
                (-> record
                    (assoc id nil)
                    (data-error {:pred `contains? :in [id]})))))]
    (let [{:keys [cells length delimiter]} format
          reader (PushbackReader. (StringReader. line))
          [valid? data] (read-record reader (char-int delimiter))
          record {:data-index index :data-line line}]
      (if valid?
        (if (and length (not= (count data) length))
          (data-error record {:pred `(length-matches? ~length)
                              :in [:data-cell]
                              :val data})
          (reduce (partial assoc-cell data) record cells))
        (data-error record {:pred `valid-cell? :in [:data-cell] :val data})))))

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
