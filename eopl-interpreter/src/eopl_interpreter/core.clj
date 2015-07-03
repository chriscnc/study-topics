(ns eopl-interpreter.core
  (require [eopl-interpreter.environment :refer [empty-env extend-env apply-env]]
           [eopl-interpreter.interpreter :refer :all])
  (:gen-class))


(defn parse-program 
  [x]
  (cond (number? x) (->Lit x)
        (symbol? x) (->Var x)
        (list? x) 
        (let [op (first x)
              rands (rest x)]
;              _ (println {:op op :rands rands})]
          (cond (= op '+)
                (->PrimApp (->AddPrim) (map parse-program rands))

                (= op '-)
                (->PrimApp (->SubPrim) (map parse-program rands))

                (= op '*)
                (->PrimApp (->MultPrim) (map parse-program rands))

                (= op 'incr)
                (let [rand-count (count rands)]
                  (if (= rand-count 1)
                    (->PrimApp (->IncrPrim) (map parse-program rands))
                    (throw (Exception. (format "Wrong number of args (%d) passed to 'incr'" rand-count)))))

                (= op 'decr)
                (let [rand-count (count rands)]
                  (if (= rand-count 1)
                    (->PrimApp (->DecrPrim) (map parse-program rands))
                    (throw (Exception. (format "Wrong number of args (%d) passed to 'incr'" rand-count)))))

                (= op 'print)
                (let [rand-count (count rands)]
                  (if (= rand-count 1)
                    (->PrimApp (->PrintPrim) (map parse-program rands))
                    (throw (Exception. (format "Wrong number of args (%d) passed to 'print'" rand-count)))))

                (= op 'minus)
                (let [rand-count (count rands)]
                  (if (= rand-count 1)
                    (->PrimApp (->MinusPrim) (map parse-program rands))
                    (throw (Exception. (format "Wrong number of args (%d) passed to 'minus'" rand-count)))))

                (= op 'list)
                (->PrimApp (->ListPrim) (map parse-program rands))

                (= op 'cons)
                (->PrimApp (->ConsPrim) (map parse-program rands))

                (= op 'car)
                (->PrimApp (->CarPrim) (map parse-program rands))

                (= op 'if)
                (let [test-exp (nth rands 0)
                      true-exp (nth rands 1)
                      false-exp (nth rands 2)]
                  (->If (parse-program test-exp)
                        (parse-program true-exp)
                        (parse-program false-exp)))

                :else (throw (Exception. (format "Invalid operator: %s" op)))))

        :else (throw (Exception. (format "Not sure what would cause the program to get here: %s" x)))))


(defn run [src-text]
  (let [env (extend-env {'x 42 'y 6} (empty-env))]
    (evaluate (parse-program src-text) env)))


(defn read-eval-print []
  (do
    (print "eopl=> ")
    (flush)
    (try
      (println (evaluate (parse-program (read-string (read-line))) (empty-env)))
      (catch Exception e 
        (println e)))
    (recur)))


(defn -main
  [& args]
  (read-eval-print))
