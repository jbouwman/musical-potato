(ns aoc2021.day14-test
  (:require [aoc2021.day14 :as sut]
            [clojure.test :as t]))

(t/deftest day14-test
  (t/is (= 2223 (sut/part-1)))
  (t/is (= 2566282754493 (sut/part-2))))
