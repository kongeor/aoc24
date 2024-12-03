(ns core.day3
  (:require [clojure.core.logic.fd :as fd]
            [clojure.java.io :as io]
            [clojure.core.logic :refer :all]
            [clojure.string :as str])
  (:refer-clojure :exclude [==]))

(defne sumo [l p]
  ([() 0])
  ([[[x y] . t] z]
   (fresh [p' s']
     (fd/* x y p')
     (fd/+ p' s' z)
     (sumo t s'))))

(comment
  (run 1 [q]
       (sumo [] q))
  (run 1 [q]
       (sumo [[3 2]] q))
  (run 1 [q]
       (sumo [[3 2] [3 3] [1 2]] q)))

(def sample "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(defn parse-input [input]
  (map (fn [[_ x y]]
         [(parse-long x)
          (parse-long y)]) (re-seq #"mul\((\d+),(\d+)\)" input)))

(comment
  (parse-input sample))

(comment
  (run 1 [q]
    (sumo (parse-input sample) q)))

;; part a
(comment
  (run 1 [q]
    (sumo (parse-input (slurp (io/resource "input3.txt"))) q))) ;; => 179571322

;; ---------------
;; part 2

(def sample2 "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(comment
  (re-seq #"mul\((\d+),(\d+)\)|(don\'t)|(do)" sample2))