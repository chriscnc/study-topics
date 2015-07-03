(ns reducibles.core
  (:require [criterium.core :as crit]
            [clojure.core.reducers :as r]))

(defn empty-range? [start end step]
  (or (and (pos? step) (>= start end))
      (and (neg? step) (<= start end))))


(defn lazy-range 
  [i end step]
  (lazy-seq (if (empty-range? i end step)
              nil
              (cons i (lazy-range (+ i step)
                                  end 
                                  step)))))
            
(lazy-range 5 10 2)
(lazy-range 6 0 -1)
(reduce conj [] (lazy-range 6 0 -1))
(reduce + 0 (lazy-range 6 0 -1))

(defn reducible-range [start end step]
  "Reducible constructor. Constructs a Reducible"
  (fn [reducing-fn init]
    (loop [result init, i start]
      (if (empty-range? i end step)
        result
        (recur (reducing-fn result i) 
               (+ i step))))))

(def countdown-reducible (reducible-range 6 0 -1))
(countdown-reducible conj [])
(countdown-reducible + 0)


(defn half [x]
  "Mapping function"
  (/ x 2))

(half 4)
(half 7)


(defn sum-half [result input]
  "Reducing function"
  (+ result (half input)))

(reduce sum-half 0 (lazy-range 0 10 2))
((reducible-range 0 10 2) sum-half 0)

(defn half-transformer [f1]
  "Reducing function transformer"
  (fn f1-half [result input]
    (f1 result (half input))))

((reducible-range 0 10 2) (half-transformer +) 0)
((reducible-range 0 10 2) (half-transformer conj) [])

(defn mapping [map-fn]
  "Transformer constructor"
  (fn map-transformer [f1]
    (fn [result input]
      (f1 result (map-fn input)))))

((reducible-range 0 10 2) ((mapping half) +) 0)
((reducible-range 0 10 2) ((mapping half) conj) [])
((reducible-range 0 10 2) ((mapping list) conj) [])


(defn filtering [filter-pred]
  (fn [f1]
    (fn [result input]
      (if (filter-pred input)
        (f1 result input)
        result))))

((reducible-range 0 10 2) ((filtering #(not= % 2)) +) 0)
((reducible-range 0 10 2) ((filtering #(not= % 2)) conj) [])

((reducible-range 0 10 2)
 ((filtering #(not= % 2))
  ((mapping half) conj))
 [])


((reducible-range 0 10 2)
 ((mapping half)
  ((filtering #(not= % 2)) conj))
 [])


(defn mapcatting [map-fn]
  (fn [f1]
    (fn [result input]
      (let [reducible (map-fn input)]
        (reducible f1 result)))))


(defn and-plus-ten [x]
  (reducible-range x (+ 11 x) 10))

((and-plus-ten 5) conj [])

((reducible-range 0 10 2) ((mapcatting and-plus-ten) conj) [])

(filter #(not= % 2)
        (map half
             (lazy-range 0 10 2)))

(defn r-map [mapping-fn reducible]
  (fn new-reducible [reducing-fn init]
    (reducible ((mapping mapping-fn) reducing-fn) init)))

(defn r-filter [filter-pred reducible]
  (fn new-reducible [reducing-fn init]
    (reducible ((filtering filter-pred) reducing-fn) init)))

(def our-final-reducible
  (r-filter #(not= % 2)
            (r-map half
                   (reducible-range 0 10 2))))

(our-final-reducible conj [])

(crit/bench
  (reduce + 0 (filter even?  (map half (lazy-range 0 (* 10 1000 1000) 2)))))
                                                        

(defn core-r-map [mapping-fn core-reducible]
  (r/reducer core-reducible (mapping mapping-fn)))

(defn core-r-filter [filter-pred core-reducible]
  (r/reducer core-reducible (filtering filter-pred)))

(reduce conj []
        (core-r-filter #(not= % 2)
                       (core-r-map half [0 2 4 6 8])))

(defn reduce-range [reducing-fn init, start end step]
  (loop [result init, i start]
    (if (empty-range? i end step)
      result
      (recur (reducing-fn result i)
             (+ i step)))))

(defn core-reducible-range [start end step]
  (reify protos/CollReduce
    (coll-reduce [this reducing-fn init]
      (reducing-range reducing-fn init, start end step))
    (coll-reduce [this reducing-fn]
      (if (empty-range? start end step)
        (reducing-fn)
        (reduce-range reducing-fn start, (+ start step) end step)))))
