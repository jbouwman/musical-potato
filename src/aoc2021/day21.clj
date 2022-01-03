(ns aoc2021.day21
  (:require [clojure.string :as str]))

;; Day 21: modulo summation

(defn die [sides]
  (map inc (iterate #(mod (inc' %) sides) 0)))

(defn rollable [die]
  (let [state (atom die)]
    (fn []
      (let [result (first @state)]
        (swap! state rest)
        result))))

(defn make-game
  "Make a game with ip initial positions using a die with ds sides and board with bs positions ans win score ws."
  [ip ds bs ws]
  {:roll (rollable (die ds))
   :roll-count 0
   :board bs
   :win ws
   :player 0
   :positions (vec (map dec ip))
   :scores (vec (take (count ip) (repeat 0)))})

(defn %step-game [{:keys [player positions scores board win roll-count] :as game} move rolls]
  (let [position (mod (+ (nth positions player) move) board)
        score (+ (inc position) (nth scores player))]
    (assoc game
           :roll-count (+ roll-count rolls)
           :winner (when (<= win score) player)
           :player (mod (inc player) (count scores))
           :positions (assoc positions player position)
           :scores (assoc scores player score))))

(defn %qstep [{:keys [player positions scores board win] :as game} move]
  (let [position (mod (+ (nth positions player) move) board)
        score (+ (inc position) (nth scores player))]
    (assoc game
           :player (mod (inc player) (count scores))
           :positions (assoc positions player position)
           :scores (assoc scores player score))))



(defn make-game
  "Make a game with ip initial positions using a die with ds sides and board with bs positions ans win score ws."
  [ip ds bs ws]
  {:board-size bs
   :initial-position ip})


(defn move-by [p m bs]
  (mod (+ p m) bs))

(defn turns-to-win [bs p w ms]
  (loop [p p s 0 t 0 [m & ms] ms]
    (cond (nil? m)
          nil
          (<= w s)
          t
          :else
          (let [p (mod (+ p m) bs)]
            (recur p (+ s (inc p)) (inc t) ms)))))



(defn move [p w s t m]
  (let [p (mod (+ p m) 10)]
    [p (+ s (inc p))
     (if (<= w s)
       t
       (inc t))]))

(def base-prob
  [[3 1]
   [4 6]
   [5 6]
   [6 1]
   [7 6]
   [8 6]
   [9 1]])

(defn moves [p w s t]
  (into {}
        (map (fn [[m v]]
               [(move p w s t m) v])
             base-prob)))

(moves 4 0 0)

;; => map of p+s => v

;; all game outcomes for starting position p with turns t



probability of position + state after n turns

position, score, turns, universes



(defn games [p n w]
  (loop [n n state {[p 0 0] 1}]
    (if (= 0 n)
      (reduce (fn [s [t v]]
                (update s t #(+ (or % 0) v)))
              {}
              (map (fn [[[p s t] v]]
                     [t v])
                   state))
      (recur (dec n)
             (reduce (fn [state [k v]]
                       (update state k (fn [x]
                                         (+ (or x 0) v))))
                     {}
                     (mapcat (fn [[[p s t] v]]
                               (moves p w s t))
                             state))))))

;; position 4 wins after k turns in v universes

(games 4 10 21)

(def p4
  {4 9747, 9 891, 5 7938, 6 6291, 3 10800, 8 2538, 7 4563, 10 27})

;; there are 10,800 universes in which '4' wins in three turns
;; there are no ways for '8' to win in 3 turns, therefore 4 wins against all possible rolls of 8
;; every time 8 rolls, a new universe is formed. how many times does 8 roll in these games?
;;
;; in a 3 turn game in which 4 wins, 8 has rolled 6 times, therefore the universe has been split 3^6
;;

(def roll-multiple (int (.pow (bigdec 3) 6)))

;;
;;

(* 10800

;; position 8 wins after k turns in v universes

(games 8 10 21)

(def p8
  {4 9747, 9 891, 5 7938, 6 6291, 3 10287, 8 2538, 7 4563, 10 27})

(vals (filter (fn [[k v]] (<p8)



(defn min-score-after [p n]
  (apply min (map second (keys (games p n)))))

(min-score-after 8 10)

(count (filter (fn [[k v]]
                 (not (<= 21 (second k))))
               (games 4 10 21)))

;; must take 10 turns for all games to complete


p0 wins after 2 turns in 100 universes
p0 wins after 3 turns in 10 universes
p1 wins after 2 turns in 50 universes
p1 wins after 3 turns in 70 universes







(frequencies
 (map (fn [ms]
        (turns-to-achieve 10 4 21 ms))
      (possible-games 9)))



[1 2 3 4 5 6 7 8 9]

  4 0
6 1 1




(count (sort-by first (into [] (frequencies (map (partial score-after 10 4) (possible-games 1))))))

1 =  7 = +  7
2 = 17 = + 10
3 = 22 = +  5
4 = 29 = +  7
5 = 36 = +  7
6 = 41 = +  5
7 = 48 = +  7





{[1 8] 1, [1 9] 1, [1 10] 1, [1 1] 1, [1 2] 1, [1 3] 1, [1 4] 1}


1 18
2 8
 3    1
 5    3
 19  1
 9   4
 13  5
 12 5
 11 6
 15 3
 7   3
 17  2
 4   1
 14  4
 10 4
 6   2
 16  2

;; summation over discrete probability
;;
;;






if I start at x, and get y^n, when do i pass 21

(defn



(defn %winner-for [game ms]
  (loop [game game ms ms]
    (cond (:winner game)
          (:winner game)
          (not (empty? ms))
          (recur (%qstep game (first ms)) (rest ms)))))

;; for every turn, a die is rolled 3 times.
;; for every roll, 3 universes are generated.
;; 3 * 3 * 3 = 27
;;
;; 27 universes per turn, each of which requires a further turn

ways in which scores are rolled:

3 = 1
4 = 6
5 = 6
6 = 1
7 = 6
8 = 6
9 = 1

3, 3, 3
3, 3, 4

(defn permute [n]
  (mapcat (fn [i]
            (map (fn [j]
                   (map (fn [k]
                          [i j k])
                        (range n)))
                 (range n)))
          (range n)))

(defn permute [n l]
  (let [p (map list (range n))]
    (if (= l 1)
      p
      (mapcat (fn [p2]
                (map (fn [p3]
                       (concat p2 p3))
                     p))
              (permute n (dec l))))))


(def gg (make-game [4 8] 100 10 21))

(partial %winner-for gg)

(defn permutation->game [p]
  (map #(nth options %) p))

(defn possible-games [maxlen]
  (map permutation->game (permute 7 maxlen)))

  (count (possible-games 7))


(take 100 (drop 1000 (map (partial %winner-for gg) (possible-games 7))))


(permute 3)


27 universes => 7



(%winner-for (make-game [4 8] 100 10 21) [3 4 5 6 7 8 9])

;; 3 rolls summing to 3

(%step-game (make-game [4 8] 100 10 21) 4 3)

(defn step-game [{:keys [roll player positions scores board win roll-count] :as game}]
  (%step-game game (+ (roll) (roll) (roll)) 3))

(defn play [game]
  (loop [game game]
    (if (:winner game)
      game
      (recur (step-game game)))))

(defn score [outcome]
  (let [loser (mod (inc (:winner outcome)) 2)]
    (* (:roll-count outcome)
       (nth (:scores outcome) loser))))
