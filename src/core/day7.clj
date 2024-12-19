(ns core.day7
  (:require
    [clojure.core.logic.fd :as fd]
    [clojure.java.io :as io]
    [clojure.core.logic :refer :all]
    [clojure.string :as str])
  (:refer-clojure :exclude [==])
  (:import (clojure.core.logic LCons)))

(def example "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")

(defn parse-input [input]
  (let [lines (str/split-lines input)]
    (map #(let [[expected nums] (str/split % #":")]
            [(parse-long expected) (mapv parse-long (str/split (str/trim nums) #"\s"))]) lines)))

(comment
  (parse-input example))

;; very interesting mistake
;; notice how the reduction will not work properly (according to spec)
;; and how the accumulator starts with 0 which is problematic for multiplications
#_(defne calibro
  [r l s o]
  ([_ () r r])
  ([_ [h . t] _ _]
   (fresh [s']
     (conde
       [(fd/+ s h s')]
       [(fd/* s h s')])
     (calibro r t s' o))))

(defne reverso [l o]
  ([() ()])
  ([[h . t]]
   (fresh [o']
     (reverso t o')
     (appendo o' [h] o))))

(comment
  (run 2 [q]
    (reverso [1 2 3] q)))

(defne calibro
  [r l o]
  #_([_ () r r])
  ([h [h] h])
  ([_ [h1 h2 . t] r]
   (fresh [s' l']
     (conde
       [(fd/+ h1 h2 s')]
       [(fd/* h1 h2 s')])
     (conso s' t l')
     (calibro r l' o)
     )
   ))

(comment
  (run 5 [q]
    (fresh [q']
      #_(reverso [81 40 27] q')
      #_(calibro 3267 q' q)
      (calibro 190 [10 10 160 10] q))))

(defn solve-line [[result nums]]
  (run 1 [q]
    (calibro result nums q)))

(comment
  (time
    (let [input #_(parse-input example) (parse-input (slurp (io/resource "input7.txt")))]
      (->> input
           (map solve-line)
           #_(filter #(> (count %) 1))
           (keep first)
           (reduce +)))))

;; first mistaken answer - see explanation above
;; 1509440454730
;; success!
;; 1399219271639

(defn conco [^LCons p]
  (let [l (.a p)
        r (.d p)
        n' (parse-long (str l r))]
    n'))

;; non-relational, see is source
;; notice that params are passed in inverse order to is
(defn lconco [p n]
  (is n p conco))

(comment
  (run 1 [q]
    (fresh [l]
      (conso 1 23 l)
      (lconco l q))))

(defne calibr2o
  [r l o]
  ([h [h] h])
  ([_ [h1 h2 . t] r]
   (fresh [s' l' p]
     (conso h1 h2 p)
     (conde
       [(fd/+ h1 h2 s')]
       [(fd/* h1 h2 s')]
       [(lconco p s')])
     (conso s' t l')
     (calibr2o r l' o))
   ))

(comment
  (run 5 [q]
    (calibr2o 7290 [6 8 6 15] q)))

(comment
  (run 5 [q]
    (conso 1 2 q)))

(defn solve-line-2 [[result nums]]
  (run 1 [q]
    (calibr2o result nums q)))

(comment
  (time
    (let [input #_(parse-input example) (parse-input (slurp (io/resource "input7.txt")))]
      (->> input
           (map solve-line-2)
           #_(filter #(> (count %) 1))
           (keep first)
           (reduce +)))))

;; yey!
;; "Elapsed time: 137378.435448 msecs"
;; => 275791737999003
