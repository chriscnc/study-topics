(ns eopl-interpreter.environment
  "The environment is represented by a list of maps")

(defn empty-env [] '())

(defn extend-env [bindings env]
  (cons bindings env))

(defn apply-env [env sym]
  (if (empty? env)
    (throw (Exception. (format "No binding for '%s'." sym)))
    (let [bindings (first env)
          encl-env (rest env)]
      (if (contains? bindings sym)
        (get bindings sym)
        (recur encl-env sym)))))
