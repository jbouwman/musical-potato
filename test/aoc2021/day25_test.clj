(ns aoc2021.day25-test
  (:require [aoc2021.day25 :as sut]
            [clojure.test :as t]))

(t/deftest day25-test
  (let [ra (sut/make-array 10 10 \.)]
    (t/is ra)))

(-> (sut/make-array 10 10 \.)
    (sut/put 1 9 \x)
    (sut/down 1 9)
    (sut/right 1 0))


(-> (sut/load-string example-text)
    (sut/down 0 0))



(let [a (-> (makev 2 2 \>)
            (putv 1 0 \.))]
  (filter (fn [[x y]]
            (can-move-right? a x y))
          (r-order a)))


(def input-file "input/25")


(def example-text
  "v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>")


(defn solve [a]
  (loop [a a n 0]
    (if-not (sut/will-move? a)
      (inc n)
      (recur (sut/cycle a) (inc n)))))

(defn cc [a n]
  (loop [a a n n]
    (if (= 0 n)
      a
      (recur (sut/cycle a) (dec n)))))


(-> (sut/load-input input-file)
    solve)

(-> (sut/load-string example-text)
    solve)


;; 515 low

#_
    cycle
    cycle
    cycle)
      rm (sut/right-moves a)
      a' (reduce (fn [a [x y]]
                   (sut/right a x y))
                 a
                 rm)]
  a')



    sut/down-moves)

#_
(can-move-right? x {:x 0 :y 0})

(move-right! x)
(move-down! x)
