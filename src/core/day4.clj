(ns core.day4
  (:require
    [clojure.core.logic.fd :as fd]
    [clojure.java.io :as io]
    [clojure.core.logic :refer :all]
    )
  (:refer-clojure :exclude [==]))

(defn xmaso [x m a s]
  (== x \X)
  (== m \M)
  (== a \A)
  (== s \S))

(defn not-xmaso [x m a s]
  (conde
    [(!= x \X)]
    [(!= m \M)]
    [(!= a \A)]
    [(!= s \S)]))

(defn xmas2o [x m a s n]
  (conde
    [(== x \X)
     (== m \M)
     (== a \A)
     (== s \S)
     (== n 1)]
    [(== x \S)
     (== m \A)
     (== a \M)
     (== s \X)
     (== n 1)]
    [(condu
       [(!= x \X) (!= x \S) (== n 0)]
       [(!= m \M) (!= m \A) (== n 0)]
       [(!= a \A) (!= a \M) (== n 0)]
       [(!= s \S) (!= s \X) (== n 0)])]))

(comment
  (run 5 [q]
    (xmas2o \. \. \. \X q))
  (run 5 [q]
    (xmas2o \X \X \M \X q))
  (run 5 [q]
    (xmas2o \X \M \A \S q)))

(defne xmaseso [l n]
  ([() 0])
  ([[x] 0])
  ([[x m] 0])
  ([[x m a] 0])
  ([[x m a s . _] _]
   (fresh [n' i t]
     (resto l t)
     (xmas2o x m a s i)
     (fd/+ n' i n)
     (xmaseso t n')
     )))

(comment
  ;; multiple runs are weird
  (run 3 [q]
    (xmaseso (seq ".XMAS...") q)))

(appendo)

(defne restotesto [l t]
  ([() ()])
  ([l' _]
   (fresh [tt t']
     (resto l' tt)
     (== tt t)
     (restotesto tt t'))))

(comment
  (run 5 [q]
    (restotesto [1 2 3 4] q)))

(comment
  (run 2 [q]
    (resto [1 2 3] q)))

(seq "XMAS...")


(def sample1 "..X...
.SAMX.
.A..A.
XMAS.S
.X....")


