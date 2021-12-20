(ns aoc2021.day13
  (:require [clojure.string :as str]))

;; Day 13: Reduction over point set

(def input-file "input/13")

(def point-re #"(\d+),(\d+)")

(defn parse-point [parser line]
  (if (empty? line)
    (assoc parser :mode :operations)
    (update parser :points conj
            (map (fn [s]
                   (Integer/parseInt s))
                 (rest (re-matches point-re line))))))

(def operation-re #"fold along ([xy])=(\d+)")

(defn parse-operation [parser line]
  (let [[_ direction n] (re-matches operation-re line)]
    (update parser :operations
            conj {:direction (keyword direction)
                  :count (Integer/parseInt n)})))

(defn parse-line [{:keys [mode] :as parser} line]
  (let [parser-fn ({:points parse-point
                    :operations parse-operation} mode)]
    (parser-fn parser line)))

(defn parse-paper [input]
  (reduce parse-line {:mode :points
                      :points #{}
                      :operations []}
          input))

(defn load-paper [f]
  (parse-paper (str/split (slurp f) #"\n")))

(defn fold-up [x y n]
  [x (if (< n y) (- (* 2 n) y) y)])

(defn fold-left [x y n]
  [(if (< n x) (- (* 2 n) x) x) y])

(defn fold [points folder n]
  (reduce (fn [points [x y]]
            (conj points (folder x y n)))
          #{} points))

(defn apply-op [paper {:keys [direction count]}]
  (update paper :points fold ({:x fold-left :y fold-up} direction) count))

(defn solve [paper]
  (reduce apply-op paper (:operations paper)))

(defn show [{:keys [size points]}]
  (let [xd (first (reverse (sort (map first points))))
        yd (first (reverse (sort (map second points))))
        a (make-array Character/TYPE (inc yd) (inc xd))]
    (doseq [y (range (inc yd))]
      (doseq [x (range (inc xd))]
        (aset a y x \ )))
    (doseq [[x y] points]
      (aset a y x \#))
    (str/join "\n" (map (fn [ca] (String. ca)) a))))

(defn part-1 []
  (let [{:keys [operations] :as paper} (load-paper input-file)]
    (count (:points (apply-op paper (first operations))))))

(defn part-2 []
  (show (solve (load-paper input-file))))
