(ns reducibles.scratch
  (:require [clojure.java.io :as io]
            [clojure.core.reducers :as r]))

(defn writeln [wtr s]
  (do
    (.write wtr s)
    (.write wtr "\n")))



(defn my-map [coll]

  (with-open [rdr (io/reader (io/resource "input.txt"))
            wtr (io/writer "output.txt")]
  (dorun
    (map #(writeln wtr %) (line-seq rdr))))

(def lzs (lazy-seq [1 2 3 4]))


