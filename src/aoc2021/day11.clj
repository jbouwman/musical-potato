(ns aoc2021.day11)

;; Day 11: Game of Life-like problem
;;
;; apply recursive state transitions to a 10x10 array

(def input-file "input/day_11.txt")

(defn line->row [l]
  (map (comp #(- % 48) int) (seq l)))

(defn read-board [f]
  (with-open [reader (clojure.java.io/reader f)]
    (into [] (mapcat line->row (line-seq reader)))))

(defn increase-energy [i]
  (vec (map inc i)))

(defn flash-updates [index item]
  (when (<= 10 item)
    (let [x (mod index 10)
          y (int (/ index 10))]
      (mapcat (fn [x2]
                (map (fn [y2]
                       (when (and (<= 0 x2 9)
                                  (<= 0 y2 9))
                         (fn [i]
                           (update i (+ x2 (* 10 y2))
                                   (if (and (= x x2)
                                            (= y y2))
                                     (fn [_] -99)
                                     inc)))))
                     (range (dec y) (inc (inc y)))))
              (range (- x 1) (inc (inc x)))))))

(defn get-flashes
  "At each coordinate, return a list of modifications to the board."
  [i]
  (filter identity
          (apply concat
                 (map-indexed flash-updates i))))

(defn apply-flashes [i]
  (loop [i i]
    (let [ff (get-flashes i)]
      (if (empty? ff)
        i
        (recur (reduce (fn [i e] (e i)) i ff))))))

(defn zero-flashed [m]
  (map #(Math/max 0 %) m))

(def next-board (comp zero-flashed apply-flashes increase-energy))

(defn make-state [board]
  {:board board
   :cycles 0
   :total-flashes 0})

(defn next-state [{:keys [board cycles total-flashes]}]
  (let [board (next-board board)
        flashed (count (filter (partial = 0) board))]
    {:board board
     :cycles (inc cycles)
     :total-flashes (+ total-flashes flashed)
     :last-flashes flashed}))

(defn run-cycles [state n]
  (loop [n n state state]
    (if (= 0 n)
      state
      (recur (dec n) (next-state state)))))

;; Solutions

(defn part-1 []                         ; -> 1562
  (let [initial-state (make-state (read-board input-file))]
    (:total-flashes (run-cycles initial-state 100))))

(defn part-2 []                         ; -> 268
  (loop [state (make-state (read-board input-file))]
    (if (= (:last-flashes state) 100)
      (:cycles state)
      (recur (next-state state)))))
