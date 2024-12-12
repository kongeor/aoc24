(ns core.day9
  (:require
    [clojure.core.logic.fd :as fd]
    [clojure.core.logic.pldb :as pldb]
    [clojure.java.io :as io]
    [clojure.core.logic :refer :all]
    [clojure.string :as str])
  (:refer-clojure :exclude [==]))

(def example "2333133121414131402")

(defn parse-line [l]
  (mapv parse-long (map str l)))

(comment
  (parse-line example))

(defne repeato [x n o]
  ([_ 0 ()])
  ([_ _ _]
   (fresh [n' o']
     (fd/+ n' 1 n)
     (conso x o' o)
     (repeato x n' o'))))

(comment
  (run 2 [q]
    (repeato 5 3 q)))

(defne expando [l n b c o]
  ([() _ _ _ ()])
  ([[h . t] _ _ _ _]
   (fresh [o' o'' n']
     (conde
       [(nilo b)
        (repeato nil h o'')
        (expando t n n nil o')]
       [(nilo c)
        (fd/+ n 1 n')
        (repeato n h o'')
        (expando t n' nil n' o')])
     (appendo o'' o' o))))

(comment
  (run 2 [q]
    (expando [2 3 4 5 6] 0 0 nil q)))

(comment
  (let [l (parse-line example)]
    (run 2 [q]
      (expando l 0 0 nil q)
      )))

(defn numo [n]
  (conda
    [(nilo n) u#]
    [s# (fd/in n (fd/interval 0 10))]))

(comment
  (run 11 [q]
    (fresh [a]
      (== q nil)
      (== a q)
      (numo a))))

;; copy-pasto from day 5
(defne ntho
  [l n o]
  ([[x . _] 0 x])
  ([[_ . t] _ _]
   (fresh [n']
     (fd/+ n' 1 n)
     (ntho t n' o))))

(defne splito [l i a x b]
  ([[h . t] 0 () h t])
  ([[h . t] _ _ _ _]
   (fd/> i 0)
   (fresh [l' i' a']
     (conso h a' a)
     (fd/+ i' 1 i)
     (splito t i' a' x b))))

(comment
  (run 2 [a x b]
    (splito [1 2 3 4] 0 a x b)))

(comment
  (run 2 [a x b]
    (splito [1 2 3 4 5] 2 a x b))
  )

(defn swapo [l i j o]
  (fresh [a b x y e1 e2 l' l'' l''' l'''']
    (fresh [n n']
      (fd/- j i n')
      (fd/+ n 1 n')
      (splito l i a e1 b)
      (splito b n x e2 y)
      (appendo a [e2] l')
      (appendo l' x l'')
      (appendo l'' [e1] l''')
      (appendo l''' y o))))

(comment
  (run 2 [q]
    (swapo [1 2 3 4 5 6 7 8] 0 1 q)))

(defn defrago [l i j o]
  (conde
    [(fd/<= j i)
     (== l o)]
    [(fresh [e1 e2 l' i' j']
       (fd/< i j)
       (ntho l i e1)
       (nilo e1)
       (ntho l j e2)
       (numo e2)
       (swapo l i j l')
       (fd/+ i 1 i')
       (fd/+ j 1 j')
       (defrago l' i' j' o)
       )]
    [(fresh [e1 e2 i']
       (fd/< i j)
       (ntho l i e1)
       (numo e1)
       (ntho l j e2)
       (numo e2)
       (fd/+ i 1 i')
       (defrago l i' j o))]
    [(fresh [e1 e2 j']
       (fd/< i j)
       (ntho l i e1)
       (nilo e1)
       (ntho l j e2)
       (nilo e2)
       (fd/+ j 1 j')
       (defrago l i j' o))]))

(defne counto [l o]
  ([() 0])
  ([[_ . t] _]
   (fresh [i]
     (fd/+ i 1 o)
     (counto t i))))

(comment
  (run 2 [q]
    (counto [1 2 3] q)))

(comment
  (let [input (parse-line example)]
    (run 2 [q]
      (fresh [q' n' n'']
        (expando input 0 0 nil q')
        (counto q' n')
        (fd/- n' 1 n'')
        (defrago q' 2 41 q)))))

(comment
  (let [input (parse-line example)]
    (run 2 [q]
      (fresh [q']
        (expando input 0 0 nil q')
        (swapo q' 0 41 q)
        #_(== q q')
        )
      )))