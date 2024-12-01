(ns core.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.core.logic :refer :all]
            [clojure.core.logic.unifier :as u]
            [clojure.core.logic.fd :as fd])
  (:refer-clojure :exclude [==]))



(def example "3   4
4   3
2   5
1   3
3   9
3   3
")

(defn parse-input [input]
  (map #(let [[s1 s2] (str/split % #"\s+")]
          [(parse-long s1) (parse-long s2)]) (str/split-lines input)))

(comment
  (parse-input example))

(comment
  (parse-input (slurp (io/resource "input1.txt"))))

(defn day1a [input]
  (reduce +
          (map (fn [x y]
                 (abs (- x y)))
               (sort (map first (parse-input input)))
               (sort (map second (parse-input input))))))

(comment
  (day1a (slurp (io/resource "input1.txt")))
  ;; => 2164381
  )

(defn day1b [input]
  (let [fr (frequencies (map second input))]
    (reduce +
            (map (fn [l]
                   (* l (get fr l 0)))
                 (map first input)))))

(comment
  (day1b (parse-input example)))

(comment
  (day1b (parse-input (slurp (io/resource "input1.txt"))))
  ;; => 20719933
  )

(comment
  (run* [q]
    (membero q [1 2 3])
    (membero q [2 3 4])))

(comment
  (run 5 [q]
    (fd/in q (fd/interval 1 10))
    (fd/> q 1)))

(defne inserto
  [x l1 l2]
  ([_ () [x]])
  ([_ [y . t1] [x y . t1]]
   (fd/>= y x))
  ([_ [y . t1] [y . t2]]
   (fd/< y x)
   (inserto x t1 t2)))

(comment
  (run 2 [q]
    (inserto 3 [1 2] q)))

(comment
  (run 2 [q]
    (inserto 3 q [1 2 3 4])))

(defne sorto
  [l1 l2]
  #_([() ()])                                               ;; will yield one extra result
  ([[x] [x]])
  ([[x . t1] _]
   (fresh [tt]
     (sorto t1 tt)
     (inserto x tt l2))))

(comment
  (run 2 [q]
    (sorto [10 5 2 3 1 7] q))
  ;; this doesn't work
  #_(run 3 [q]
    (sorto q [10 5 2 3 1 7])))

#_(run 1 [q]
    (== {:a q} {:a 1}))

(defne freqio
  [l f]
  ([[x] [[x 1]]])
  ([[x . t] [[x n]]]
   (fresh [n' f']
     (fd/+ n' 1 n)
     (== f' [[x n']])
     (freqio t f')))
  ([[x . t] [[y _] . t3]]
   (fresh [f']
     (conso [x 1] f' f)
     (freqio t f'))))

(comment
  (run 3 [q]
    (freqio [1 1 2 3 3 3 4 4] q)))