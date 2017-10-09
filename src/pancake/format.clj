(ns pancake.format
  (:require [clojure.spec.alpha :as s]))

(defn unreserved? [id]
  (not (#{:data-index :data-line :data-errors} id)))

(s/def ::id (s/and (s/or :string string?
                         :keyword keyword?)
                   unreserved?))
(s/def ::description string?)
(s/def ::length integer?)
(s/def ::type #{"fixed-width" "delimited"})

;; fixed-width
(defn fixed-width-min-length [format]
  (->> format :fields (map :end) (apply max)))

(defn valid-fixed-width-length? [format]
  (let [length (:length format)
        min-length (fixed-width-min-length format)]
    (or (not length)
        (>= length min-length))))

(defn fixed-width? [format] (= (:type format) "fixed-width"))

(s/def ::start integer?)
(s/def ::end integer?)
(s/def ::field (s/keys :req-un [::id ::start ::end]))
(s/def ::fields (s/coll-of ::field))

(s/def ::fixed-width-format (s/and (s/keys :req-un [::id ::description ::type ::fields]
                                    :opt-un [::length])
                            fixed-width?
                            valid-fixed-width-length?))

;; delimited
(defn delimited-min-length [format]
  (->> format :cells (map :index) (apply max) (inc)))

(defn valid-delimited-length? [format]
  (let [length (:length format)
        min-length (delimited-min-length format)]
    (or (not length)
        (>= length min-length))))

(defn delimited? [format] (= (:type format) "delimited"))

(s/def ::index integer?)
(defn one-char? [s] (= (count s) 1))
(s/def ::delimiter (s/or :char char?
                         :string (s/and string? one-char?)))
(s/def ::cell (s/keys :req-un [::id ::index]))
(s/def ::cells (s/coll-of ::cell))
(s/def ::delimited-format (s/and (s/keys :req-un [::id
                                                  ::description
                                                  ::type
                                                  ::delimiter
                                                  ::cells]
                                         :opt-un [::length])
                                 delimited?
                                 valid-delimited-length?))
(s/def ::format (s/or :fixed-width ::fixed-width
                      :delimited ::delimited))

(defn validate-fixed-width [format] (s/explain-data ::fixed-width-format format))

(defn validate-fixed-width! [format]
  (when-let [data (validate-fixed-width format)]
    (throw (ex-info "Invalid fixed-width format." data))))

(defn validate-delimited [format] (s/explain-data ::delimited-format format))

(defn validate-delimited! [format]
  (when-let [data (validate-delimited format)]
    (throw (ex-info "Invalid delimited format." data))))
