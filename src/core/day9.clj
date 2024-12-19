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
    [(fd/in n (fd/interval 0 10))]))

(comment
  (run 11 [q]
    (fresh [a]
      (numo 1))))

;; copy-pasto from day 5
(defne ntho
  [l n o]
  ([[x . _] 0 x])
  ([[_ . t] _ _]
   (fresh [n']
     (fd/+ n' 1 n)
     (ntho t n' o))))

(comment
  (run 3 [q]
    (ntho q 3 3)))

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
    (splito [1 2 3 4 :4 5 6] x a :4 b)))

(comment
  (run 10 [x]
    (splito x 2 [1 2] 3 [4 5 6 7]))
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

;; attempt 2

(comment
  (let [l (parse-line example)]
    (run 2 [q]
      (expando l 0 0 nil q)
      )))

(defne merglos [l1 l2 o]
  ([_ () l2])
  ([[h1 . t1] [h2 . t2] _]
   (fresh [o']
     (merglos t1 t2 o')
     (conde
       [(numo h1)
        (conso h1 o' o)]
       [(nilo h1)
        (conso h2 o' o)]))))

(comment
  (run 2 [q]
    #_(merglos [] [] q)
    (merglos [0   0   nil nil 1   1   nil]
             [nil nil 10  11  nil nil 12]
             q)))

(comment
  (let [a '(0   0   nil nil nil 1   1   1   nil nil nil 2   nil nil nil 3 3 3 nil 4 4 nil 5 5 5 5 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil)
        b '(nil nil 9   9   8   nil nil nil 8   8   8   nil nil 7 7 7 nil nil nil nil 6 nil nil 6 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil)]
    (run 2 [q]
      (merglos a b q))))

(defn calc1o [l i j o n]
  (conde
    [(fd/<= j i)
     (== o [])
     (== n i)]
    [(fd/> j i)
     (fresh [a b o']
       (ntho l i a)
       (ntho l j b)
       (conde
         [(numo a)
          (numo b)
          (fresh [i']
            (fd/+ i 1 i')
            (conso nil o' o)
            (calc1o l i' j o' n))]
         [(numo a)                                          ;; todo dup
          (nilo b)
          (fresh [i']
            (fd/+ i 1 i')
            (conso nil o' o)
            (calc1o l i' j o' n))]
         [(nilo a)
          (numo b)
          (fresh [i' j']
            (fd/+ i 1 i')
            (fd/- j 1 j')
            (conso b o' o)
            (calc1o l i' j' o' n))]
         [(nilo a)
          (nilo b)
          (fresh [j']
            (fd/- j 1 j')
            #_(conso b o' o)                                ;; funky bug!
            (calc1o l i j' o n))]))]))

(defne takeo [l n o]
  ([_ 0 ()])
  ([[h . t] _ _]
   (fresh [n' o']
     (fd/+ n' 1 n)
     (conso h o' o)
     (takeo t n' o'))))

(comment
  (run 2 [q]
    (takeo [1 2 3 4] 2 q)))

(defne checksumo                                            ;; the famous warrior!
  [l i o]
  ([() _ 0])
  ([[h . t] _ _ ]
   (fresh [i' s o']
     (fd/+ i 1 i')
     #_(fd/dom s (fd/interval 0 100000000000))
     (fd/* i h s)
     (checksumo t i' o')
     (fd/+ s o' o))))

(comment
  (run 2 [q]
    (checksumo [1 0 2 3 2  4 4 43 2 2] 0 q)))


(comment
  (count (butlast (parse-line (slurp (io/resource "input9.txt"))))))

(comment
  (let [
        ; input (parse-line example)
        input (butlast (parse-line (slurp (io/resource "input9.txt"))))

        ; input [0 0 nil nil nil 1 1 1 nil nil nil 2 nil nil nil 3 3 3 nil 4 4 nil 5 5 5 5 nil 6 6 6 6 nil 7 7 7 nil 8 8 8 8 9 9]
        ; j (dec (count input))
        ]
    (run 2 [q ]
      (fresh [x z j j' z z' z'' q' q'' q''' q'''' q''''' n n']
        (expando input 0 0 nil x)
        (counto x j')
        (fd/+ j 1 j')                                       ;; it's inverse but all right
        (calc1o x 0 j q' n)
        (fd/+ n n' j)
        (takeo x n z)
        (repeato nil n' z')
        (appendo z z' z'')
        (takeo q' n q'')
        (repeato nil n' q''')
        (appendo q'' q''' q'''')
        #_(== q'''' q)
        #_(== q n )
        #_(== q' q)
        #_(merglos z'' q'''' q''''')
        (merglos z'' q'''' q)

        ;; why this complaints about the domain?
        #_(checksumo q''''' 0 q)

        #_(calc1o input 0 j q n)))))

#_(count '(0 0 nil nil nil 1 1 1 nil nil nil 2 nil nil nil 3 3 3 nil 4 4 nil 5 5 5 5 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil))


#_(count '(nil nil 9 9 8 nil nil nil 8 8 8 nil nil 7 7 7 nil nil nil nil 6 nil nil 6 nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil))

;; attempt 3

(defn calc1oo [l i j n o]
  (conde
    [(fd/<= j i)
     (fd/== i n)
     (== o [])]
    [(fd/<= j i)
     (fd/!= i n)
     (fresh [o' i']
       (fd/+ i 1 i')
       (conso nil o' o)
       (calc1oo l i' j n o'))]
    [(fd/> j i)
     (fresh [a b o']
       (ntho l i a)
       (ntho l j b)
       (conde
         [(numo a)
          (fresh [i']
            (fd/+ i 1 i')
            (conso a o' o)
            (calc1oo l i' j n o'))]
         [(nilo a)
          (numo b)
          (fresh [i' j']
            (fd/+ i 1 i')
            (fd/- j 1 j')
            (conso b o' o)
            (calc1oo l i' j' n o'))]
         [(nilo a)
          (nilo b)
          (fresh [j']
            (fd/- j 1 j')
            #_(conso b o' o)                                ;; funky bug!
            (calc1oo l i j' n o))]))]))

(comment
  (let [
        ; input (parse-line example)
        ; n (dec (count input))
        input (butlast (parse-line (slurp (io/resource "input9.txt"))))

        ; input [0 0 nil nil nil 1 1 1 nil nil nil 2 nil nil nil 3 3 3 nil 4 4 nil 5 5 5 5 nil 6 6 6 6 nil 7 7 7 nil 8 8 8 8 9 9]
        ; j (dec (count input))
        ]
    (println input)
    (run 2 [q]
      (fresh [j x x' j']
        (expando input 0 0 nil q)
        #_(counto x j)
        #_(fd/+ j' 1 j)                                     ;; it's inverse but all right
        #_(calc1oo x 0 j' j' x')
        #_(== q x')
        #_(checksumo x' 0 q)

        #_(counto x q)
        )
      )))

(comment
  (run 10 [a b]
    (appendo a b [1 2 3 4])))

(defne identityo [l o]
  ([() ()])
  ([[h . t] _]
   (fresh [t']
     (conso h t' o)
     #_(== t' t)
     (identityo t' t)
     )))

(defn identity2o [l o]
  (conde
    [(== l [])
     (== o [])]
    [(fresh [a d o']
       (conso a d l)
       (conso a o' o)
       (identity2o d o'))]))

(comment
  (reduce +
          (butlast (parse-line (slurp (io/resource "input9.txt"))))))

(comment
  (time
    (let [input
          (butlast (parse-line (slurp (io/resource "input9.txt"))))]
      (doall
        (run 2 [q]
          #_(ntho input 19998 q)
          #_(identity2o input q)
          (identityo input q))))))
