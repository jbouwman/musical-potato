(ns aoc2021.day17
  (:require [clojure.string :as str]))

(def example
  "target area: x=20..30, y=-10..-5")

;; x and y are not independent, but x has a minimum value that will get the probe to the target, varying by y velocity.
;;
;; compute possible x velocities that intersect: f


(def target-re #"target area: x=(-?\p{Digit}+)\.\.(-?\p{Digit}+), y=(-?\p{Digit}+)\.\.(-?\p{Digit}+)")

(defn parse-target [s]
  (map #(Integer/parseInt %) (rest (re-matches target-re s))))

;; (parse-target example)

t = 0
x = 0
v = 10

t = 1
x = 10
v = 9

t = 2
x = 19 = (20 - 1)
v = 8

t = 3
x = 27 = (30 - 3) 3 = 2 + 1
v = 7

t = 4
x = 34 = (40 - 6) 6 = 3 + 2 + 1
v = 6

t = 5
x = 40 = (50 - 10) 10 = 4 + 3 + 2 + 1
v = 5

t = 6
x = 45 = (60 - 15) S { 1 ... 5}
v = 4

t = 7
x = 49 = (70 - 21) S6
v = 3

t = 8
x = 52
v = 2

t = 9
x = 54
v = 1

t = 10
x = 55
v = 0

max dist for velocity of 10 is 55

(defn x-at-time [xv t]
  (let [t (min xv t)
        u (dec t)]
    (- (* t xv)
       (/ (* u (+ 1 u)) 2))))

(x-at-time 10 9)

max range is gotten at t = initial velocity


(defn x-at-time [v t]
  (let [t (min v t)
        u (dec t)]
    (- (* t xv)
       (/ (* u (+ 1 u)) 2))))

(defn y-at-time [v t]
  (let [u (dec t)]
    (- (* t v)
       (/ (* u (+ 1 u)) 2))))

(defn xv [[xs xe]]
  (->> (range 1 (inc xe))
       (map #(vector % (x-at-time % %)))
       (filter #(<= xs (second %)))
       (map first)))

(defn hits? [target xv yv]
  (let [[x1 x2 y1 y2] target]
    (loop [t 1]
      (let [x (x-at-time xv t)
            y (y-at-time yv t)]
        (cond (and (<= x1 x x2)
                   (<= y1 y y2))
              true
              (or (< x2 x)
                  (< y y1))             ; y grows upward
              nil
              :else
              (recur (inc t)))))))


(y-at-time 121 121)

(defn trajectories [target]
  (let [[x1 x2 y1 y2] target
        xvs (xv [x1 x2])]
    (filter :hit?
            (mapcat (fn [xv]
                      (map (fn [yv]
                             {:xv xv
                              :yv yv
                              :hit? (hits? target xv yv)})
                           (range y1 1000)))
                    xvs))))

(def puzzle (parse-target "target area: x=185..221, y=-122..-74"))

(count (trajectories puzzle))

(def actual
  (set (map (fn [{:keys [xv yv]}]
              [xv yv])
            (trajectories (parse-target example)))))

(def actual
  (set (map (fn [{:keys [xv yv]}]
              [xv yv])
            (trajectories (parse-target example)))))

(count (trajectories (parse-target example)))

(apply disj expect actual)




(xv [20 30])

(def expect #{
              [10 -1]
              [10 -2]
              [11 -1]
              [11 -2]
              [11 -3]
              [11 -4]
              [12 -2]
              [12 -3]
              [12 -4]
              [13 -2]
              [13 -3]
              [13 -4]
              [14 -2]
              [14 -3]
              [14 -4]
              [15 -2]
              [15 -3]
              [15 -4]
              [20 -10]
              [20 -5]
              [20 -6]
              [20 -7]
              [20 -8]
              [20 -9]
              [21 -10]
              [21 -5]
              [21 -6]
              [21 -7]
              [21 -8]
              [21 -9]
              [22 -10]
              [22 -5]
              [22 -6]
              [22 -7]
              [22 -8]
              [22 -9]
              [23 -10]
              [23 -5]
              [23 -6]
              [23 -7]
              [23 -8]
              [23 -9]
              [24 -10]
              [24 -5]
              [24 -6]
              [24 -7]
              [24 -8]
              [24 -9]
              [25 -10]
              [25 -5]
              [25 -6]
              [25 -7]
              [25 -8]
              [25 -9]
              [26 -10]
              [26 -5]
              [26 -6]
              [26 -7]
              [26 -8]
              [26 -9]
              [27 -10]
              [27 -5]
              [27 -6]
              [27 -7]
              [27 -8]
              [27 -9]
              [28 -10]
              [28 -5]
              [28 -6]
              [28 -7]
              [28 -8]
              [28 -9]
              [29 -10]
              [29 -5]
              [29 -6]
              [29 -7]
              [29 -8]
              [29 -9]
              [30 -10]
              [30 -5]
              [30 -6]
              [30 -7]
              [30 -8]
              [30 -9]
              [6 0]
              [6 1]
              [6 2]
              [6 3]
              [6 4]
              [6 5]
              [6 6]
              [6 7]
              [6 8]
              [6 9]
              [7 -1]
              [7 0]
              [7 1]
              [7 2]
              [7 3]
              [7 4]
              [7 5]
              [7 6]
              [7 7]
              [7 8]
              [7 9]
              [8 -1]
              [8 -2]
              [8 0]
              [8 1]
              [9 -1]
              [9 -2]
              [9 0]
              })
