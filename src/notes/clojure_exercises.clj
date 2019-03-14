(ns notes.clojure-exercises
  (:use [anglican.runtime :exclude [sum]]))

;;;; exercise 1: summing values ;;;;

(defn sum
  "returns the sum of values in a sequence"
  [vals]
  (loop [result 0.0
         values vals]
    (if (seq values)
      (recur (+ result (first values))
             (rest values))
      result)))

(sum [1 2 3]) ;; => 6.0

;; the above sum function returns a double, so it doesn't preserve type
(defn sum-two
  [vals]
  (loop [result nil
         values vals]
    (if (seq values)
      (if result
        (recur (+ result (first values))
               (rest values))
        (recur (first values)
               (rest values)))
        result)))

(sum-two [1.0 2.0 3.0]) ;; => 6.0
(sum-two [1 2 3]) ;; => 6

;; as a reduce
(defn sum-three
  [vals]
  (reduce + vals))

(sum-three [1.0 2.0 3.0]) ;; => 6.0
(sum-three [1 2 3]) ;; => 6

;; now write a cumulative sum function
(defn cumsum
  "returns a vector of partial sums"
  [vals]
  (loop [results nil
         values vals]
    (if (seq values)
      (if results
        (recur (conj results
                     (+ (last results) (first values)))
               (rest values))
        (recur (vector (first values))
               (rest values)))
      results)))

(cumsum (list 1 2 3 4)) ;; => [1 3 6 10]

;;;; exercise 2: higher-order functions ;;;;

(defn my-mapv
  [f vals]
  (loop [results []
         values vals]
    (if (seq values)
      (recur (conj results (f (first values)))
             (rest values))
      results)))

(my-mapv #(* % %)
         (range 5)) ;; => [0 1 4 9 16]

(defn my-map
  [f vals]
  (lazy-seq
   (when (seq vals)
     (conj (my-map f (rest vals))
           (f (first vals))))))

(my-map #(* % %) (range 5)) ;; => (0 1 4 9 16)
(my-map #(* % %) nil) ;; => ()

;; composition
(defn my-comp [f g]
  (fn [x]
    (f (g x))))

(let [f sqrt
      g (fn [x] (* x x))
      h (my-comp f g)]
  (h -10)) ;; => 10

;; generalize to accepting multiple functions
(let [f (fn [& args]
          (prn args))]
  (f 1)
  (f 2 3 4))

(let [args [1 2 3]]
  (apply + args))

(defn my-comp [f g]
  (fn [args]
    (f (apply g args))))

(let [f (comp abs *)]
  (f -1 2 -3 4 5)) ;; => 120

;; reduce

(defn my-reduce
  ([f init values]
   (if (seq values)
     (f (my-reduce f init (rest values))
        (first values))
     init))
  ([f values]
   (my-reduce f (first values) (rest values))))

(my-reduce + [1 2 3 4]) ;; => 10
(my-reduce + 0 [1 2 3 4]) ;; => 10

(my-reduce (fn [sums v]
             (conj sums (if (seq sums)
                          (+ (peek sums) v)
                          v)))
           []
           [1 2 3 4]) ;; => [4 7 9 10]

;; the above is a right fold
(defn my-foldl
  [f init values]
  (if (seq values)
    (my-foldl f (f init (first values)) (rest values))
    init))

(my-foldl + 0 [1 2 3 4]) ;; => 10

(my-foldl (fn [sums v]
            (conj sums (if (seq sums)
                         (+ (peek sums) v)
                         v)))
          []
          [1 2 3 4]) ;; => [1 3 6 10]

;;;; exercise 3: recursion ;;;;

(defn numerify
  "try to parse a string as a number, do nothing when 
  the string is does not parse to a number or for any
  other type of object"
  [v]
  (cond
    (string? v) (read-string v)
    (list? v) (list (map numerify v))
    (vector? v) (mapv numerify v)
    (seq? v) (map numerify v)
    (map? v) (into {} (map numerify v))
    :else v))

;; tests
(numerify "hello")
(numerify "-3.14159e0")
(numerify ["1" "2" "3"])
(numerify (list "1" 2 "3"))
(numerify [["1" 2] [3 "4"]])
(numerify {1 "2" "3" 4 :a 5})

(class (list "1" 2 "3"))
(list? (numerify (list "1" 2 "3")))

