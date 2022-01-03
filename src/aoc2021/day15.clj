(ns aoc2021.day15
  (:require [clojure.string :as str]))

;; Day 15: Shortest path search

(def input-file "input/15")

(def example-text
  ["1163751742"
   "1381373672"
   "2136511328"
   "3694931569"
   "7463417111"
   "1319128137"
   "1359912421"
   "3125421639"
   "1293138521"
   "2311944581"])

(defn line->row [l]
  (into [] (map (fn [c]
                  (- (int c) 48))
                (seq l))))

(defn parse-input [text]
  (into [] (map line->row text)))

(defn inc-mod [n m i]
  (inc (mod (dec (+ n (mod i m))) m)))

(defn inc-map [m n]
  (map (fn [row]
         (map (fn [px]
                (inc-mod px 9 n))
              row))
       m))

(defn expand-map [m x]
  (let [added (map (partial inc-map m)
                   (range 1 x))
        wider (reduce (partial map concat) m added)
        added (map (partial inc-map wider)
                   (range 1 x))]
    (vec (map vec (reduce concat wider added)))))

(expand-map [[1]] 1)

(defn load-input [f]
  (parse-input (str/split (slurp f) #"\n")))

(defn cave-vertices [cave]
  (let [width (count (first cave))
        height (count cave)]
    (set (mapcat (fn [y]
                   (map (fn [x]
                          [x y])
                        (range width)))
                 (range height)))))

;; (cave-vertices (parse-input example-text))

(defn cave-neighbors [cave [x y]]
  (let [width (count (first cave))
        height (count cave)]
    (cond-> #{}
      (< 0 x) (conj [(dec x) y])
      (< (inc x) width) (conj [(inc x) y])
      (< 0 y) (conj [x (dec y)])
      (< (inc y) height) (conj [x (inc y)]))))

;; (cave-neighbors (parse-input example-text) [0 0])

(defn set-intersect [s s']
  (set (filter (fn [e] (contains? s e)) s')))

(defn min-dist [q d]
  (second
   (first
    (sort-by first
             (filter first
                     (map (fn [[k v]]
                            [(:dist v) k])
                          (select-keys d q)))))))

(defn find-path [cave position]
  (let [q (cave-vertices cave)
        d (reduce (fn [d v]
                     (assoc d v {:dist nil :prev nil}))
                   {}
                   q)
        d (assoc-in d [position :dist] 0)]
    (loop [d d q q]
      (when (= 0 (mod (count q) 100))
        (println (format "%d vertices" (count q))))
      (if (empty? q)
        d
        (let [u (min-dist q d)
              q (disj q u)
              n (set-intersect q (cave-neighbors cave u))]
          (recur
           (reduce (fn [d v]
                     (let [alt (+ (get-in d [u :dist]) (get-in cave v))
                           vd (get-in d [v :dist])]
                       (if (or (not vd) (< alt vd))
                         (assoc d v {:dist alt :prev u})
                         d)))
                   d
                   n)
           q))))))

(defn solve [input]
  (let [d (find-path input [0 0])]
    (:dist (get d [(dec (count (first input))) (dec (count input))]))))

(solve (expand-map (load-input input-file) 5))

(time (solve (expand-map (parse-input example-text) 8)))
