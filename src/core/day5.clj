(ns core.day5
  (:require
    [clojure.core.logic.fd :as fd]
    [clojure.core.logic.pldb :as pldb]
    [clojure.java.io :as io]
    [clojure.core.logic :refer :all]
    [clojure.string :as str])
  (:refer-clojure :exclude [==]))

(def sample "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")


(defn parse-input [input]
  (let [lines (str/split-lines input)
        rules (take-while #(not (str/blank? %)) lines)
        pages (drop-while #(not (str/blank? %)) lines)
        ]
    [(map #(let [[x y] (str/split % #"\|")]
             [(parse-long x) (parse-long y)]) rules)
     (map #(->> (str/split % #",")
                (map parse-long)) (rest pages))]))

(parse-input sample)

#_(pldb/db-rel pages p1 p2)
(pldb/db-rel pages ^:index p1 ^:index p2)

(defn make-facts [rules]
  (reduce (fn [db [x y]]
            (pldb/db-fact db pages x y))
          (pldb/db)
          rules))

(defn beforo [p1 p2]
  (fresh [p]
    (conde
      [(pages p1 p2)]
      [(pages p1 p)
       (beforo p p2)]
      )))

(defne ntho
  [l n o]
  ([[x . _] 0 x])
  ([[_ . t] _ _]
   (fresh [n']
     (fd/+ n' 1 n)
     (ntho t n' o))))

(comment
  (run 1 [q]
    (ntho [1 2 3 4] 3 q)))

(defn solve-line [facts nums]
  (let [
        ;nums [75, 47, 61, 53, 29]
        ; nums [75 29 13]
        middle-idx (long (/ (count nums) 2))
        vars (repeatedly (count nums) lvar)
        pairs (partition 2 1 vars)]
    (pldb/with-db facts
      (run 1 [q]
        #_(== q vars)
        (ntho vars middle-idx q)
        (and* (map (fn [n v] (== n v)) nums vars))
        (everyg (fn [[x y]]
                   (beforo x y)) pairs)
        ))))

(comment
  (time
    (let [[rules pages] (parse-input (slurp (io/resource "input5.txt")))
          facts (make-facts rules)]
      (->> pages
           (map #(let [solution (solve-line facts %)]
                   (println "sol>" solution)
                   solution))
           (keep seq)
           (map first)
           (reduce +)))))

(comment
  (map (fn [[x y]]
         [x y]) (partition 2 1 [1 2 3 4])))

(comment
  (map (fn [x y]
         [x y]) (repeat 5 1) (range 10)))