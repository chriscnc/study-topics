(ns sicp.scratch)

(defn vrange [n]
  (loop [i 0
         v []]
    (if (< i n)
      (recur (inc i) (conj v i))
      v)))

(defn vrange2 [n]
  (loop [i 0 
         v (transient [])]
    (if (< i n)
      (recur (inc i) (conj! v i))
      (persistent! v))))

;(time (def v (vrange 1000000)))
;(time (def v2 (vrange2 1000000)))

(def v (atom []))
(defn insert-queue [v e]
  (swap! v conj e))
(defn front-queue [v]
  (let [e (first @v)]
    (swap! v rest)
    e))




