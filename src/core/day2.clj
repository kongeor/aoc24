(ns core.day2
  (:require [clojure.core.logic.fd :as fd]
            [clojure.java.io :as io]
            [clojure.core.logic :refer :all]
            [clojure.string :as str])
  (:refer-clojure :exclude [==]))


(defn parse-num-lines [input]
  (mapv #(let [tokens (str/split % #"\s+")]
          (mapv parse-long tokens)) (str/split-lines input)))

(comment
  (parse-num-lines
    (slurp (io/resource "input2.txt"))))

(def sample "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

(comment
  (parse-num-lines sample))

(defn diffo [x y]
  (fresh [z]
    (fd/- y x z)
    (membero z [1 2 3])))

(comment
  (run 5 [q]
    (diffo 1 2)))

(defne monotono
  [q]
  ([()]
   s#)
  ([[x]]
   s#)
  ([[x . t]]
   (fresh [y]
     (firsto t y)
     (conde
       [(fd/>= y x)
        (diffo x y)]
       [(fd/<= y x)
        (diffo y x)])
     (monotono t))))

(comment
  (run 1 [q]
    #_(monotono [1])
    (monotono [1 3 2 4 5])
    #_(monotono [6 5 2])
    #_(monotono [2 5 6])))

(comment
  (mapv
    #(run 1 [q]
      (monotono %))
    (parse-num-lines sample)))