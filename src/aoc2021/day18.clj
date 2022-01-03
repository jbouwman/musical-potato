(ns aoc2021.day18
  (:require [clojure.string :as str]))

;; Day 18: list reduction

(defn permute [n]
  (mapcat (fn [i]
            (map (fn [j]
                   [i j])
                 (range (inc i) n)))
          (range n)))

(defn index-of [f s]
  (some identity
        (map-indexed (fn [i n]
                       (when (f n) i))
                     s)))

(defn getn [n p]
  (case (first p)
    :l (getn (first n) (rest p))
    :r (getn (second n) (rest p))
    nil n))

(defn updaten [n p f]
  (case (first p)
    :l [(updaten (first n) (rest p) f) (second n)]
    :r [(first n) (updaten (second n) (rest p) f)]
    nil (f n)))

(defn magnitude [n]
  (if (number? n)
    n
    (+ (* 3 (magnitude (first n)))
       (* 2 (magnitude (second n))))))

(defn walk
  ([n d]
   (cond (number? n)
         [(assoc d :value n)]
         (coll? n)
         (let [d (update d :depth inc)]
           (concat (walk (first n)
                         (update d :path conj :l))
                   (walk (second n)
                         (update d :path conj :r))))))
  ([n]
   (walk n {:depth 0 :path []})))

(defn path->index [p]
  (loop [[x & p] p i 1]
    (case x
      nil i
      :l (recur p i)
      :r (recur p (inc i)))))

(defn explode [p n]
  (let [[a b] (getn n p)
        n' (updaten n p (fn [_] 0))
        rewalked (walk n')
        i (index-of #(= (:path %) p) rewalked)]
    (cond-> n'
      (< 0 i)
      (updaten (:path (nth rewalked (dec i)))
               (partial + a))
      (< (inc i) (count rewalked))
      (updaten (:path (nth rewalked (inc i)))
               (partial + b)))))

(defn split [p n]
  (updaten n p
           (fn [x]
             (let [q (quot x 2)
                   r (rem x 2)]
               [q (+ q r)]))))

(defn next-action [n]
  (let [walked (walk n)]
    (or (some (fn [{:keys [depth path]}]
                (when (= 5 depth)
                  (partial explode (take (dec (count path)) path))))
              walked)
        (some (fn [{:keys [path value]}]
                (when (<= 10 value)
                  (partial split path)))
              walked))))

(defn reducen [n]
  (loop [n n]
    (if-let [f (next-action n)]
      (recur (f n))
      n)))
