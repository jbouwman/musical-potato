(ns aoc2021.day25
  (:require [clojure.string :as str]))

;; Day 25: automata

(defn makev [w h v]
  (vec (take h (repeat (vec (take w (repeat v)))))))

(defn putv [a x y v]
  (assoc-in a [y x] v))

(defn getv [a x y]
  (get-in a [y x]))

(defn down [a x y]
  (let [v (getv a x y)
        y' (mod (inc y) (count a))
        v' (get-in a [y' x])]
    (-> a
        (putv x y v')
        (putv x y' v))))

(defn right [a x y]
  (let [v (getv a x y)
        x' (mod (inc x) (count (first a)))
        v' (get-in a [y x'])]
    (-> a
        (putv x y v')
        (putv x' y v))))

(defn load-input [f]
  (vec (map vec (str/split (slurp f) #"\n"))))

(defn load-string [s]
  (vec (map vec (str/split s #"\n"))))

(defn widthv [a]
  (count (first a)))

(def heightv count)

(defn right-of [a x]
  (mod (inc x) (widthv a)))

(defn left-of [a x]
  (mod (dec x) (widthv a)))

(defn below [a y]
  (mod (inc y) (heightv a)))

(defn above [a y]
  (mod (dec y) (heightv a)))

(defn rangev [a]
  (mapcat (fn [y]
            (map (fn [x]
                   [x y])
                 (range (widthv a))))
          (range (heightv a))))

(defn %cycle [a testf updatef]
  (reduce (fn [a [x y]] (updatef a x y))
          a
          (filter (fn [[x y]] (testf a x y))
                  (rangev a))))

(defn will-move? [a]
  (some (fn [[x y]]
          (or (can-move-right? a x y)
              (can-move-down? a x y)))
        (rangev a)))

(defn emptyv? [a x y]
  (= \. (getv a x y)))

(defn can-move-right? [a x y]
  (and (= \> (getv a x y))
       (emptyv? a (right-of a x) y)))

(defn can-move-down? [a x y]
  (and (= \v (getv a x y))
       (emptyv? a x (below a y))))

(defn cycle-right [a]
  (%cycle a can-move-right? right))

(defn cycle-down [a]
  (%cycle a can-move-down? down))

(defn cycle [a]
  (-> a cycle-right cycle-down))
