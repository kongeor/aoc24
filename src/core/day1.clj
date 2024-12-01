(ns core.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))



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