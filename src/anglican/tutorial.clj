(ns anglican.tutorial
  (:require [anglican importance lmh]
            [anglican.stat :as stat])
  (:use [anglican core runtime emit]))

;; basics

;; Anglican has a number of random primitives, like normal-- calling (normal mu std) creates a distribution
;; object, which can be sampled from.
(sample* (normal 1 2))
;; coin flip
(sample* (flip 0.7))
;; other distributions
(sample* (uniform-continuous 3 10))
(sample* (beta 2 3))
(sample* (binomial 10 0.4))
(sample* (discrete [0.3 0.2 0.5]))

(let [normal-dist (normal 1 2.2)]
  (repeatedly 10 (fn [] (sample* normal-dist))))

;; observe gives us log probabilities of a distribution at a value
(observe* (normal 0 1) 3)

;; example query) let
;;     theta ~ Beta(5, 3)
;;     x     ~ Bernoulli(theta)
;; we ask: p(theta > 0.7 | x = true)

(defquery one-flip [outcome]
  (let [theta (sample (beta 5 3))]
    (observe (flip theta) outcome)
    (predict :theta-gt-pt07 (> theta 0.7))))

;;;; more queries

(defquery one-flip [outcome]
  (let [bias (sample (uniform-continuous 0.0 1.0))]
    (observe (flip bias) outcome)
    bias))

(def samples
  (doquery :rmh one-flip [true]))

(first samples)