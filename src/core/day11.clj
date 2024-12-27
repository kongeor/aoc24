(ns core.day11
  (:require
    [clojure.core.logic.fd :as fd]
    [clojure.core.logic.pldb :as pldb]
    [clojure.java.io :as io]
    [clojure.core.logic :refer :all]
    [clojure.string :as str])
  (:refer-clojure :exclude [==]))

(def example "125 17")

(defn digits [n]
  (count (seq (str n))))

(comment
  (digits 100))

(defn digitso [n o]
  (is o n digits))

(comment
  (run 2 [q]
    (digitso "123" q)))

(defn mod2o [n d]
  (is d n (fn [x] (mod x 2))))

(comment
  (run 2 [n]
    (mod2o 9 n)))

(defn splito [n o]
  (is o n (fn [n]
            (let [sn (str n)
                  c (count sn)
                  ; _ (println "->" sn c)
                  n' (mapv (fn [t]
                             (parse-long (apply str t))) (split-at (/ c 2) sn))]
              [(first n') (second n')]))))

(comment
  (split-at 2 "1000"))

(comment
  (apply str (seq "100")))

(comment
  (run 2 [q]
    (splito "1005" q)))

(defn *2024o [n o]
  (is o n #(* 2024 %)))

(comment
  (run 2 [q]
    (*2024o 2 q)))

(defn converto [n o]
  (conde
    [(== n 0)
     (== o [1])]
    [(fresh [d m]
       (digitso n d)
       (mod2o d m)
       (== m 0)
       (splito n o))]
    [(fresh [d m x]
       (!= n 0)
       (digitso n d)
       (mod2o d m)
       (== m 1)
       (*2024o n x)
       (== [x] o))]))

(comment
  (run 2 [q]
    (converto 17 q)))

;; will not work as non relational
#_(defn concato [xs ys o]
  (is o xs #(concat % ys)))

(comment
  (run 2 [q]
    (concato [1 2 3] [4 5 6] q)))

(defne solvo [l o]
  ([() ()])
  ([[h . t] _]
   (fresh [x o']
     (converto h x)
     (solvo t o')
     (appendo x o' o)
     #_(concato x o' o)
     )))

(comment
  (run 2 [q]
    (conjo [1 2 3] [4] q)))

(comment
  (run 2 [q]
    (solvo [253 0 2024 14168] q)
    #_(solvo [253000 1 7] q)
    #_(solvo [125 17] q)))


(defn do-it [input]
  (first
    (run 2 [q]
      (solvo input q))))

(comment
  (do-it [125 17]))

(defn part1 [input n]
  (loop [in input
         n n]
    #_(println n in)
    (if (= n 0)
      in
      (recur (do-it in) (dec n)))))

(comment
  (time
    (count (part1 [125 17] 2))))

(comment
  (time
    ;; timings between different jvms
    ;; 23: 7859.438767 msecs
    ;; 21: 11260.697612
    (count (part1 [125 17] 20))))

(defn parse-input [input]
  ;; use \s+ instead of ' ' to avoid \n issues
  (keep parse-long (str/split input #"\s+")))

(comment
  (let [nums (parse-input (slurp (io/resource "input11.txt")))]
    (time
      (count (part1 nums 20)))))

;; work one element at a time for maximum speed!
(comment
  ;; "Elapsed time: 984279.902817 msecs"
  ;; => 182721
  ;; there ^ is one forgotten element from my puzzle input there
  ;; "Elapsed time: 24799.471129 msecs"
  ;; => 11836
  ;; (+ 11836 182721)
  ; => 194557 \o/
  (let [nums (parse-input (slurp (io/resource "input11.txt")))]
    (time
      (reduce + (map #(do
                        (println "working on" %)
                        (count (part1 [%] 25))) [85629] #_nums)))))
