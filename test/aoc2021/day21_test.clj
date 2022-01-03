(ns aoc2021.day21-test
  (:require [aoc2021.day21 :as sut]
            [clojure.string :as str]
            [clojure.test :as t]))

(def input-file "input/21")

(def initial-positions [8 4])

(t/deftest part-1-test
  (t/is (= 504972
           (sut/score (sut/play (sut/make-game initial-positions 100 10 1000))))))

(t/deftest part-2-test
  (let [nn (test-input)]
    (t/is (= 1 1))))
