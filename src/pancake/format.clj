(ns pancake.format
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]))

(def reserved-ids #{:data-index :data-line :data-cell :data-errors
                    "data-index" "data-line" "data-cell" "data-errors"})
;; predicates
(defn unreserved? [id]
  (not (contains? reserved-ids id)))

;; fixed-width predicates
(defn fixed-width-min-length [format]
  (->> format :fields (map :end) (apply max)))

(defn valid-fixed-width-length? [format]
  (let [length (:length format)
        min-length (fixed-width-min-length format)]
    (or (not length)
        (>= length min-length))))

(defn fixed-width? [format] (= (:type format) "fixed-width"))

(defn end-after-start? [field]
  (> (:end field) (:start field)))

;; specs
(s/def ::id (s/with-gen (s/and string? (complement str/blank?))
              #(s/gen #{"test"})))
(s/def ::description string?)
(s/def ::type #{"fixed-width" "delimited"})

(s/def ::generic-spec (s/or :qualified-keyword qualified-keyword?
                            :function fn?))
(s/def ::spec ::generic-spec)

(s/def ::keyword-or-populated-string?
  (s/or :keyword keyword?
        :string (s/and string? (complement str/blank?))))

;; fixed-width specs
(s/def :pancake.fixed-width/record-length pos-int?)
(s/def :pancake.fixed-width/id ::keyword-or-populated-string?)
(s/def :pancake.fixed-width/start nat-int?)
(s/def :pancake.fixed-width/end pos-int?)
(s/def :pancake.fixed-width/spec ::generic-spec)
(s/def :pancake.fixed-width/field (s/and (s/keys :req-un [:pancake.fixed-width/id
                                                          :pancake.fixed-width/start
                                                          :pancake.fixed-width/end]
                                                 :opt-un [:pancake.fixed-width/spec])
                      end-after-start?))
(s/def :pancake.fixed-width/fields (s/+ :pancake.fixed-width/field))

(s/def :pancake.fixed-width/format (s/and (s/keys :req-un [::id
                                                           ::description
                                                           ::type
                                                           :pancake.fixed-width/fields]
                                                  :opt-un [:pancake.fixed-width/record-length ::spec])
                                          fixed-width?
                                          valid-fixed-width-length?))

;; delimited predciates
(defn one-char? [s] (= (count s) 1))

(defn delimited-min-length [format]
  (->> format :cells (map :index) (apply max) (inc)))

(defn valid-delimited-length? [format]
  (let [length (:length format)
        min-length (delimited-min-length format)]
    (or (not length)
        (>= length min-length))))

(defn delimited? [format] (= (:type format) "delimited"))

;; delimited specs
(s/def :pancake.delimited/cell-count pos-int?)
(s/def :pancake.delimited/id ::keyword-or-populated-string?)
(s/def :pancake.delimited/index (s/int-in 0 1000))
(s/def :pancake.delimited/spec ::generic-spec)

(s/def :pancake.delimited/delimiter (s/or :char char?
                                          :string (s/and string? one-char?)))
(s/def :pancake.delimited/cell (s/keys :req-un [:pancake.delimited/id
                                                :pancake.delimited/index]
                                       :opt-un [:pancake.delimited/spec]))
(s/def :pancake.delimited/cells (s/+ :pancake.delimited/cell))
(s/def :pancake.delimited/format (s/and (s/keys :req-un [::id
                                                         ::description
                                                         ::type
                                                         :pancake.delimited/delimiter
                                                         :pancake.delimited/cells]
                                                :opt-un [:pancake.delimited/cell-count ::spec])
                                        delimited?
                                        valid-delimited-length?))

;; format spec
(s/def :pancake/format (s/or :fixed-width :pancake.fixed-width/format
                             :delimited :pancake.delimited/format))

;; functions
(defn validate-fixed-width [format] (s/explain-data :pancake.fixed-width/format format))

(defn validate-fixed-width! [format]
  (when-let [data (validate-fixed-width format)]
    (throw (ex-info "Invalid fixed-width format." data))))

(defn validate-delimited [format] (s/explain-data :pancake.delimited/format format))

(defn validate-delimited! [format]
  (when-let [data (validate-delimited format)]
    (throw (ex-info "Invalid delimited format." data))))

(defn validate [format] (s/explain-data :pancake/format format))
(defn validate! [format]
  (when-let [data (validate format)]
    (throw (ex-info "Invalid format." data))))

(defn value-specs
  [format]
  (->> (get format (case (:type format)
                     "delimited" :cells
                     "fixed-width" :fields
                     :else (throw (ex-info (str "Invalid format type: " (:type format) ".") format))))
       (filter :spec)
       (map #(vector (:id %) (:spec %)))
       (into {})))

(s/fdef value-specs
  :args (s/cat :format :pancake/format)
  :ret (s/map-of ::id ::generic-spec))
