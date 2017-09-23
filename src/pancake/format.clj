(ns pancake.format
  (:require [clojure.spec.alpha :as s]))

(defn unreserved? [id]
  (not (#{:data-index :data-line :data-errors} id)))

(s/def ::id (s/and (s/or :string string?
                         :keyword keyword?)
                   unreserved?))
(s/def ::description string?)
(s/def ::length integer?)
(s/def ::type #{"flat" "delimited"})

;; flat
(defn flat-min-length [format]
  (->> format :fields (map :end) (apply max)))

(defn valid-flat-length? [format]
  (let [length (:length format)
        min-length (flat-min-length format)]
    (or (not length)
        (>= length min-length))))

(defn flat? [format] (= (:type format) "flat"))

(s/def ::start integer?)
(s/def ::end integer?)
(s/def ::field (s/keys :req-un [::id ::start ::end]))
(s/def ::fields (s/coll-of ::field))

(s/def ::flat-format (s/and (s/keys :req-un [::id ::description ::type ::fields]
                                    :opt-un [::length])
                            flat?
                            valid-flat-length?))

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
(s/def ::cell (s/keys :req-un [::id ::index]))
(s/def ::cells (s/coll-of ::cell))
(s/def ::delimited-format (s/and (s/keys :req-un [::id ::description ::type ::cells]
                                         :opt-un [::length])
                                 delimited?
                                 valid-delimited-length?))
(s/def ::format (s/or :flat ::flat
                      :delimited ::delimited))

(defn validate-flat [format] (s/explain-data ::flat-format format))

(defn validate-flat! [format]
  (when-let [data (validate-flat format)]
    (throw (ex-info "Invalid flat format." data))))

(defn validate-delimited [format] (s/explain-data ::delimited-format format))

(defn validate-delimited! [format]
  (when-let [data (validate-delimited format)]
    (throw (ex-info "Invalid delimited format." data))))
