(ns pancake.factory)

(defn fields-from-lengths
  [lengths]
  (:fields (reduce
            (fn [{:keys [id start fields]} length]
              (let [next-start (+ start length)
                    end (dec next-start)
                    field {:id (str (char id)) :start start :end end}]
                {:id (inc id)
                 :start next-start
                 :fields (conj fields field)}))
            {:id 65 :start 1 :fields []}
            lengths)))
