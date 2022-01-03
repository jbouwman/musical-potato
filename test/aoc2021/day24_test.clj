(ns aoc2021.day24-test
  (:require [aoc2021.day24 :as sut]
            [clojure.test :as t]
            [clojure.string :as str]))

;; only inputs are w and z
;;
;; w = 1...9
;; z = ?

(def digits (partition 18
                       (str/split (slurp "input/24") #"\n")))

(defn run-1 [z i]
  (:z (sut/run-prog (assoc (sut/init [i]) :z z)
                    (sut/load-string digit-1))))

(defn run-2 [z]
  (map #(run-1 z %) (range 1 10)))

(run-2 -2)

(sut/trace-prog digit-1 [9] {})

[9 9 9 9 9 9 9 9 9 9 9 9 9 9]

(sut/trace-prog (slurp "input/24") [1 3 5 7 9 2 4 6 8 9 9 9 9 9]
                {})



(defn run-digit [d z i]
  (:z (sut/run-prog (assoc (sut/init [i]) :z z)
                    (sut/load-inst (nth digits d)))))

(filter #(= 0 (second %))
        (map (fn [z]
               [z
                (run-digit 13 z 7)]) (range -10000 10000)))


-14, 12 + 7 ==



;; for a given digit, for each possible input value, what are acceptable values
;; of z that produce n?

(defn run-digit [d z i]
  (:z (sut/run-prog (assoc (sut/init [i]) :z z)
                    (sut/load-inst (nth digits d)))))

(defn sweep-digit [d z']
  (into {}
        (map (fn [i]
               [i
                (map first
                     (filter (fn [[z r]]
                               (= r z'))
                             (map (fn [z]
                                    [z
                                     (run-digit 12 z i)])
                                  (range -1000 1000))))])
             (range 1 10))))

(map concat
(map vals
(map (fn [z]
       (sweep-digit 12 z))
     (apply concat (vals (sweep-digit 13 0))))))

;; {7 (-5 21), 1 (-11 15), 4 (-8 18), 6 (-6 20), 3 (-9 17), 2 (-10 16), 9 (-3 23), 5 (-7 19), 8 (-4 22)}

(sweep-digit 12 -5)

;; {7 (567), 1 (561), 4 (564), 6 (566), 3 (563), 2 (562), 9 (569), 5 (565), 8 (568)}

(sweep-digit 11 -135)



(run-1 26 8)

  (map (fn [n]
       (sut/trace-prog digit-1 [n] {:z 199}))
     (range 1 10))



(def ex1 "inp x
mul x -1")

(sut/trace-prog ex2 [3 9])

(t/deftest day25-ex1-test
  (let [prog (sut/load-string ex1)
        alu (sut/init [2])]
    (t/is (= -2 (:x (sut/run-prog alu prog))))))

(def ex2 "inp z
inp x
mul z 3
eql z x")

(t/deftest day25-ex2-test
  (let [prog (sut/load-string ex2)
        alu1 (sut/init [3 9])
        alu2 (sut/init [5 2])]
    (t/is (= 1 (:z (sut/run-prog alu1 prog))))
    (t/is (= 0 (:z (sut/run-prog alu2 prog))))))

(def ex3 "inp w
add z w
mod z 2
div w 2
add y w
mod y 2
div w 2
add x w
mod x 2
div w 2
mod w 2")

(t/deftest day25-ex3-test
  (let [prog (sut/load-string ex3)
        alu (sut/init [15])]
    (t/is (= (assoc (zipmap [:w :x :y :z] (repeat 1)) :input nil)
             (sut/run-prog alu prog)))))

(def input-file "input/24")

(sut/trace-prog (slurp input-file)
                (take 14 (repeat 1)))

(defn pmn [n]
  (map (comp #(- % 48) int) n))

(def mmn (vec (take 14 (repeat 1))))

(defn mns [mn]
  (vector (quot (mod mn 100000000000000) 10000000000000)
          (quot (mod mn 10000000000000) 1000000000000)
          (quot (mod mn 1000000000000) 100000000000)
          (quot (mod mn 100000000000) 10000000000)
          (quot (mod mn 10000000000) 1000000000)
          (quot (mod mn 1000000000) 100000000)
          (quot (mod mn 100000000) 10000000)
          (quot (mod mn 10000000) 1000000)
          (quot (mod mn 1000000) 100000)
          (quot (mod mn 100000) 10000)
          (quot (mod mn 10000) 1000)
          (quot (mod mn 1000) 100)            ; 10 ^ 2
          (quot (mod mn 100) 10)              ; 10
          (mod mn 10)))                       ; 1

(def mn-min 11111111111111)

(def mn-max 99999999999999)

(defn mnseq []
  (filter (fn [mn]
            (not (contains? (into #{} mn) 0)))
          (map mns (range mn-min mn-max))))

(map run-monad (take 10 (mnseq)))

(def monad (sut/load-input input-file))

(sut/trace-prog (sut/init (first (mnseq))) monad)


(defn run-monad [mn]
  (:z (sut/run-prog (sut/init mn) monad)))

(some (fn [mn]
        (when (= 0 (:z (run-monad mn)))
          mn))
      (take 5000000 (mnseq)))


(mns (inc (inc mmn)))

(bit-shift-right 255 6)

(defn mni->sq [mn]
  (bit-shift-right mn



mmn

mmn

(pmn "13579246899999")


;; => {:w 9, :x 1, :y 23, :z 2608302421, :input nil}
(let [prog (sut/load-input input-file)
      alu (sut/init [1 3 5 7 9 2 4 6 8 9 9 9 9 9])]
  (sut/run-prog alu prog));; => {:w 9, :x 1, :y 23, :z 2608302421, :input nil}
