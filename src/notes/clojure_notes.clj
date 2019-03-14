(ns notes.clojure-notes
    (:use [anglican.runtime :exclude [sum]]))

;; arithmetic
(+ 1 1)
(- 10 3)
(* 4 (- (+ 2 1) 4) 1)
(/ 10 3)   ;; just returns 10/3
(/ 10 3.0) ;; this returns 3.333...

;; anglican gives us these special functions (coming from java)
(log 3)
(exp -2)

;; get documentation
(clojure.repl/doc +)

;; get source
(clojure.repl/source +)

;; if statements
(if (> 3 2) 1 -1)
(+ (if (< 4 5) 1 2) 3)

;; nil is a logical false
(if nil true false)
;; all else is true
(if "" true false)
(if [] true false)
(if 0 true false)

;; let blocks allow definitions of local closures
(let [x 10 
      y 2]
    (+ (* x 3) y))

;; side-effects in clojure
(let [x 10
      y 2]
    (println "x times 3 = " (* x 3))
    (+ x y))

;; functions can be given two ways-- as anonymous lambdas
((fn [x y] (+ (* 2 x) y 3)) 5 10)

;; or as named functions
(defn my-fn [x y]
    (+ (* 2 x) y 3))

(my-fn 5 10)

;; even more anonymously
(let [f #(+ (* 2 %1) %2 3)]
    (f 5 10))

;; macros don't evaluate-- they transform code at compile-time
(macroexpand '(when 1 2 3)) ;; the quote in front tells us to not eval
;; the above evals to (if 1 (do 2 3))

;; evaluation macros
(let [c 5]
    (-> c (+ 3) (/ 2) (- 1))) ;; thread-first

(let [c 5]
    (macroexpand '(-> c (+ 3) (/ 2) (- 1))))

(let [c 5]
    (->> c (+ 3) (/ 2) (- 1)))

(let [c 5]
    (macroexpand '(->> c (+ 3) (/ 2) (- 1))))

;;;; data structures ;;;;

;; lists
(list 1 2 3)
(seq? (list 1 2 3))
(list? (seq [1 2 3]))

(first (list 1 2 3))
(rest (list 1 2 3))

(cons 0 (list 1 2 3))

(conj (list 1 2 3) 0) ;; this acts different for vectors
(peek (list 1 2 3))

(pop (list 1 2 3))

(repeat 5 (+ 1 1))

(range 5)
(range 2 8)

(repeatedly 3 (fn [] (+ 10 20)))

;; vectors
[1 2 3]

(conj [1 2 3] 4) ;; unlike lists, appends to back
(cons 0 [1 2 3])

(last [1 2 3])

(let [v [1 2 3]]
    (v 2)) ;; as a function, vectors are !! (get nth value)

;; hash maps
{:a 1 :b 2 :c 3}

(keyword "a")
(class :a) ;; clojure.lang.Keyword class

{:a 1 "b" 2 [3 4] 5} ;; but keys could be anything

(get {:a 1 :b 2 :c 3} :a) ;; getter
(assoc {:a 1 :b 2 :c 3} :d 4) ;; setter

(seq {:a 1 :b 2 :c 3})

;; hash maps have two different variants
;; array maps are ordered
(let [m (array-map :a 1 :b 2 :c 3)]
    (first m)) ;; [:a 1]

;; hash maps aren't
(let [m (hash-map :a 1 :b 2 :c 3)]
    (first m)) ;; [:c 3]

;;;; laziness ;;;;

;; clojure supports lazy paradigms, but explicitly
(defn inf-range
    "returns an infinite range using lazy evaluation"
    ([start step]
        (lazy-seq
            (cons start
                (inf-range (+ start step) step))))
    ([step]
        (inf-range 0 step))
    ([]
        (inf-range 0 1)))

;;;; higher-order functions ;;;;

;; map
(map (fn [x] (* x x))
     (list 1 2 3 4))

(map #(pow % 2)
     (range 1 5))

;; map = mapWith in clojure
(map (fn [x y] (+ x (* 2 y)))
     [1 2 3]
     [10 9 8])

;; reduce
(reduce + 0.0 [1 2 3])

(reduce + [1 2 3])

(reduce (fn [y x]
          (if (> x 0)
            (conj y x)
            y))
        []
        [-1 1 -2 2 -3 3])

;; filter
;; the above was a filter function
(filter #(> % 0) [-1 1 -2 2 -3 3])

;;;; loops ;;;;

;; loops are written using loop ... recur

(loop [x 1]
  (if (<= x 10)
    (let [next-x (+ x 1)]
      (println x)
      (recur next-x))))

(loop [x 10
       y []]
  (if (= x 0)
    y
    (recur (- x 1) (conj y (* 2 x)))))