(ns bayesian.arithmetic
  (:require [anglican.runtime :refer [sample* observe* flip uniform-discrete discrete normal log exp abs]]))

(def log-prob observe*)

;; operations
(defn safe-div
  [x y]
  (if (zero? y) 0 (/ x y)))

(def operations ['+ '- '* 'safe-div])

;; generative model for functions of the form (fn [x] f a b) where a, b are either numeric constants or x
(defn get-int-constant []
  (sample* (uniform-discrete 0 10)))

(defn sample-operation []
  (get operations (sample* (uniform-discrete 0 (count operations)))))

(defn sample-symbol []
  (if (sample* (flip 0.6)) (get-int-constant) 'x))

(defn sample-simple []
  (list (sample-operation) (sample-symbol) (sample-symbol)))

;; we can sample from the above
(def ten-samples (repeatedly 10 sample-simple))

;; and evaluate them
(defn make-fn [body-expr]
  (list 'fn ['x] body-expr))

(def eval-random-fn
  (let [example-fn (make-fn (sample-simple))]
    (println example-fn)
    (println "evaluated at x=5:" ((eval example-fn) 5))))
