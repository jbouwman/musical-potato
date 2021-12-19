(ns aoc2021.day11-test
  (:require [aoc2021.day11 :as sut]
            [clojure.test :as t]))


(t/deftest day11-test
  (t/is (= 1562 (sut/part-1)))
  (t/is (= 268 (sut/part-2))))
