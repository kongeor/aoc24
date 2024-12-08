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

(comment
  (let [nums [97,13,75,29,47]
        vars (repeatedly (count nums) lvar)
        fvar (first vars)
        var-pairs (partition 2 1 vars)
        facts (make-facts (first (parse-input sample)))]
    #_(clojure.pprint/pprint facts)
    (pldb/with-db facts
      (run 5 [q]
        (== fvar (first nums))
        (== q vars)
        (everyg (fn [[x y]]
                  (pages x y)) var-pairs)
        ))))

#_(pldb/db-rel pages p1 p2)
(pldb/db-rel pages ^:index p1 ^:index p2)

(defn make-facts [rules]
  (reduce (fn [db [x y]]
            (pldb/db-fact db pages x y))
          (pldb/db)
          rules))

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
  (let [middle-idx (long (/ (count nums) 2))
        vars (repeatedly (count nums) lvar)
        pairs (partition 2 1 vars)]
    (pldb/with-db facts
      (run 1 [q]
        (ntho vars middle-idx q)
        (and* (map (fn [n v] (== n v)) nums vars))
        (everyg (fn [[x y]]
                   (pages x y)) pairs)
        ))))

(comment
  (time
    (let [[rules pages] (parse-input #_sample (slurp (io/resource "input5.txt")))
          facts (make-facts rules)]
      #_(def facts facts)
      #_(def page-pairs pages)
      (->> pages
           (map #(let [solution (solve-line facts %)]
                   solution))
           (keep seq)
           (map first)
           (reduce +))))                                    ;; => 4872
  )

(defn solve-line-2 [facts nums]
  (let [middle-idx (long (/ (count nums) 2))
        vars (repeatedly (count nums) lvar)
        pairs (partition 2 1 vars)]
    (pldb/with-db facts
      (run 10 [q]
        #_(ntho vars middle-idx q)
        (fresh [p]
          #_(permuteo nums q)
          (== q vars)
          (everyg (fn [[x y]]
                    (membero x nums)
                    (membero y nums)
                    (pages x y)) pairs))

        ))))

(comment
  (let [[rules pages] (parse-input #_sample (slurp (io/resource "input5.txt")))
        facts (make-facts rules)]
    ;; lol, super slow
    (solve-line-2 facts [100000, 33, 79, 45, 75, 95, 17, 51, 94, 36, 34, 25, 47, 24, 55, 16, 53, 86, 57, 85]))
  )

(defn solvo-lino-2 [nums o]
  (conde
    [(fresh [x nums']
       (conso x nums' nums)
       (emptyo nums')
       (== [x] o))]
    [(fresh [x y nums' nums'' o']
       (rembero x nums nums')
       (rembero y nums' nums'')
       (pages x y)
       (solvo-lino-2 nums' o')
       (conso x o' o))]))

(comment
  (let [[rules pages1] (parse-input sample #_(slurp (io/resource "input5.txt")))
        facts (make-facts rules)]
    #_(clojure.pprint/pprint rules)
    (pldb/with-db facts
      (run 5 [q]
        (solvo-lino-2 [75, 97, 47, 61, 53] q)
        #_(solvo-lino-2 [75 97 47] q)
        ))))


(defn solvolineooo-2 [l o]
  (fresh [x l' y l'' o']
    (rembero x l l')
    (rembero y l' l'')
    (pages x y)
    (conso x o' o)
    (conda
      [(emptyo l'')
       (== [x y] o)]
      [(solvolineooo-2 l' o')])))

(comment
  (let [[rules pages1] (parse-input sample #_(slurp (io/resource "input5.txt")))
        facts (make-facts rules)
        l [75 97 47 61 53]]
    (pldb/with-db facts
      (run 5 [q]
        (solvolineooo-2 l q)
        ))))

(comment
  (run 2 [q]
    (solvo-lino-2 [1 2] q)))

(comment
  (let [l [1]]
    (run 2 [q]
      (fresh [d]
        (resto l d)
        (emptyo d)))))

(comment
  (let [l [1 2 3 4]]
    (run 20 [q]
      (fresh [x l' y l'']
        (rembero x l l')
        (rembero y l' l'')
        (== x 4)
        (== [x y] q)))))

(comment
  (time
    (let [[rules pages] (parse-input #_sample (slurp (io/resource "input5.txt")))
          facts (make-facts rules)]
      #_(def facts facts)
      #_(def page-pairs pages)
      (println "count > " (count pages))
      (->> #_pages [#_[33,24,16,31,47,21,73,36,57,86,94] [33,79,45,75,95,17,51,94,36,34,25,47,24,55,16,53,86,57,85]]
           ;; as lazy as it gets
           ;; bear with me, it's late
           (map #(let [s1 (solve-line facts %)
                       s2 nil #_(solve-line-2 facts %)]
                   (println "*" s1)
                   [s1 %]))
           (filter (comp empty? first))
           (map #(solve-line-2 facts (second %)))
           #_(reduce +))))                                    ;; => 4872
  )

(defne incro
  [l]
  ([()]
   s#)
  ([[h]]
   s#)
  ([[f . t]]
   (do
     (println f)
     s#)
   (fresh [s]
     (firsto t s)
     (fd/+ f 1 s)
     (incro t))))

(comment
  (run 2 [q]
    (== [1 q] [1 3])
    #_(incro q)))

(comment
  (let [nums [1 2 3 4 6]]
    ;; with incro and 2 it hangs, why?
    (run 2 [q]
      (fresh [h h']
        (firsto nums h)
        (firsto q h')
        (== h h')
        (== q [1 2 3 4 6])
        (permuteo nums q)))))

(comment
  (let [nums (nth page-pairs 2)]
    (pldb/with-db facts
      (run 5 [q]
        (pages 75 q)))))

(comment
  (nth page-pairs 1))

(comment
  (let [nums (nth page-pairs 2)
        index (get-in facts ["core.day5/pages_2" 0])
        after (fn [n]
                 (->> (get index n)
                      (mapv second)
                      set)
                 )]
    (after (first nums))
    ))

(comment
  (map (fn [[x y]]
         [x y]) (partition 2 1 [1 2 3 4])))

(comment
  (map (fn [x y]
         [x y]) (repeat 5 1) (range 10)))

