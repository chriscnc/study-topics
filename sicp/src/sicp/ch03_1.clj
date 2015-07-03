(ns sicp.ch03-1)

(def balance (atom 100))

(defn withdraw [amount]
  (if (>= @balance amount)
    (swap! balance - amount)
    "Insufficient funds"))

(defn new-withdraw []
  (let [balance (atom 100)]
    (fn [amount]
      (if (>= @balance amount)
        (swap! balance - amount)
        "Insufficient funds"))))

;(def w (new-withdraw))
;(w 45)

(defn make-withdraw [balance]
  (let [b (atom balance)]
  (fn [amount]
    (if (>= @b amount)
      (swap! b - amount)
      "Insufficient funds"))))

(def W1 (make-withdraw 100))
(def W2 (make-withdraw 100))
;(W1 50)
;(W2 35)

(defn make-account [balance pass]
  (let [b (atom balance)
        failed-attempts (atom 0)]
    (letfn [(withdraw [amount]
              (if (>= @b amount)
                (swap! b - amount)
                "Insufficient funds"))
            (deposit [amount]
              (swap! b + amount))
            (dispatch [password op]
              (if (not= password pass)
                (if (> @failed-attempts 3)
                  (fn [arg] "Account locked-out")
                  (fn [arg] (do (swap! failed-attempts inc)
                                "Incorrect password")))
                (do (reset! failed-attempts 0)
                    (cond (= op 'withdraw) withdraw
                          (= op 'deposit) deposit
                          :else (throw (Exception. (str "Unknown request -- MAKE-ACCOUNT" op)))))))]
      dispatch)))

(def acc (make-account 100 'secret))
((acc 'secret 'withdraw) 40)
((acc 'wrong-pass 'withdraw) 40)

; Exercise 3.1
(defn make-accumulator [init]
  (let [acc (atom init)]
    (fn [arg] (swap! acc + arg))))

(def A (make-accumulator 5))
;(A 10)

; Exercise 3.2
(defn make-monitored [f]
  (let [cnt (atom 0)]
    (letfn [(how-many-calls? [] @cnt)
            (reset-count! [] (reset! cnt 0))
            (dispatch [arg]
              (cond (= arg 'how-many-calls?) (how-many-calls?)
                    (= arg 'reset-count!) (reset-count!)
                    :else (do (swap! cnt inc)
                              (f arg))))]
      dispatch)))

(def i (make-monitored inc))
;(i 100)
;(i 'how-many-calls?)
;(i 'reset-count!)

; Exercises 3.3-3.4 - modified inline above
