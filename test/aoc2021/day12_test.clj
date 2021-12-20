(ns aoc2021.day12-test
  (:require [aoc2021.day12 :as sut]
            [clojure.test :as t]))

(t/deftest day12-test
  (t/is (= 3679 (sut/part-1)))
  (t/is (= 107395 (sut/part-2))))
