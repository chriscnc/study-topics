(ns sicp.streams)

(take 10 (repeatedly (constantly 1)))

(take 10 (map repeatedly (constantly 1)))
(lazy-seq
