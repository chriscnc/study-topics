(ns sicp.ch01-1-test
  (:require [midje.sweet :refer :all]
            [sicp.ch01-1 :refer :all]))


(fact "sum-larger-of works"
  (sum-larger-of 1 2 3) => 13
  (sum-larger-of 2 3 1) => 13
  (sum-larger-of 3 1 2) => 13)

