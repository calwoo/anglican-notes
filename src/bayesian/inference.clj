(ns bayesian.inference
  (:require [anglican.runtime :refer [mean]]
            [gorilla-plot.core :as plot]))

;; normal distribution
(defn normal-logpdf
  [x mu sigma]
  (if (> sigma 0)
    (anglican.runtime/observe* (anglican.runtime/normal mu sigma) x)
    (Math/log 0.0)))

(defn randn [mu sigma]
  (anglican.runtime/sample* (anglican.runtime/normal mu sigma)))

(defn resample
  [N values weights]
  (let [dist (anglican.runtime/categorical (map list values weights))]
    (repeatedly N #(anglican.runtime/sample* dist))))

;;;; exercise 1: importance sampling ;;;;

;; We are given the generative model
;;    sigma^2 = 2
;;    mu   ~ Normal(1, \sqrt(5))
;;    y|mu ~ Normal(mu, sigma)  
;;    observe: y = 9, 8


(def N 1000)

(def dataset [9 8])

(defn sample-from-proposal
  "sample from a proposal distribution q(mu) = p(mu)"
  []
  (randn 1 (anglican.runtime/sqrt 5)))

(defn compute-unnormalized-log-weight
  "return the log likelihood of the data, given a mean mu:
   log p(y1, y2| mu)"
  [mu]
  (let [sigma (anglican.runtime/sqrt 2)
        like-y1 (normal-logpdf (get dataset 0) mu sigma)
        like-y2 (normal-logpdf (get dataset 1) mu sigma)]
    (+ like-y1 like-y2)))

; draw N samples from proposal distribution
(def proposal-samples
  (repeatedly N sample-from-proposal))

; compute unnormalized weights for each sample
(def unnormalized-log-weights
  (map compute-unnormalized-log-weight proposal-samples))

; normalize the unnormalized log weights
(defn normalize
  [log-w]
  (let [denominator (Math/log (reduce + (map #(Math/exp %) log-w)))]
    (map #(Math/exp (- % denominator)) log-w)))

(def normalized-weights (normalize unnormalized-log-weights))

(take 10 normalized-weights)

;; estimate of mu
(def x-hat (reduce + (map * proposal-samples normalized-weights)))
;; estimate of sigma
(def x-squared-hat (let [squares (map #(* % %) proposal-samples)]
                     (reduce + (map * squares normalized-weights))))
(def est-sigma (Math/sqrt (- x-squared-hat (* x-hat x-hat))))


;;;; exercise 2: markov chain monte carlo

(def measurements [-27.020 3.570 8.191 9.898 9.603 9.945 10.056])

;; here we have measurements of x from 7 scientists
;; to model this, suppose we have dumb uninformative priors
;;      mu      ~ Normal(0, 50)
;;      sigma_i ~ Uniform(0, 25)
;;      y_i     ~ Normal(mu, sigma_i) for each i

;; question) what is the posterior distribution of x?

