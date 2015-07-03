(ns eopl-interpreter.interpreter
  (require [eopl-interpreter.environment :refer [empty-env extend-env apply-env]])
  (:gen-class))


(defprotocol Primitive
  (apply-primitive [this args]))

(defrecord AddPrim [])
(defrecord SubPrim [])
(defrecord MultPrim [])
(defrecord IncrPrim [])
(defrecord DecrPrim [])
(defrecord PrintPrim [])
(defrecord MinusPrim [])
(defrecord ListPrim [])
(defrecord ConsPrim [])
(defrecord CarPrim [])


(extend-protocol Primitive
  AddPrim
    (apply-primitive [this args] (apply + args))
  SubPrim
    (apply-primitive [this args] (apply - args))
  MultPrim
    (apply-primitive [this args] (apply * args))
  IncrPrim
    (apply-primitive [this args] (inc (first args)))
  DecrPrim
    (apply-primitive [this args] (dec (first args)))
  PrintPrim
    (apply-primitive [this args] (println (first args)))
  MinusPrim
    (apply-primitive [this args] (- (first args)))
  ListPrim
    (apply-primitive [this args] (apply list args))
  ConsPrim
    (apply-primitive [this args] (cons (first args) (second args)))
  CarPrim
    (apply-primitive [this args] (first (first args)))
)
    
(defn true-value? [x]
  (not (zero? x)))

(defprotocol Expression
  (evaluate [this env]))

(defrecord Lit [datum])
(defrecord Var [id])
(defrecord PrimApp [prim rands])
(defrecord If [test-exp true-exp false-exp])

(extend-protocol Expression
  Lit
    (evaluate [this env] (let [datum (:datum this)]
                           datum))
  Var
    (evaluate [this env] (let [id (:id this)]
                           (apply-env env id)))
  If
    (evaluate [this env] (let [test-exp (:test-exp this)
                               true-exp (:true-exp this)
                               false-exp (:false-exp this)
                               test-exp-eval (evaluate test-exp env)]
                           (if (true-value? test-exp-eval)
                             (evaluate true-exp env)
                             (evaluate false-exp env))))
  PrimApp
    (evaluate [this env] (let [prim (:prim this)
                               rands (:rands this)
                               args (map #(evaluate % env) rands)]
                           (apply-primitive prim args))))

