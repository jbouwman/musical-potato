(ns aoc2021.day18-test
  (:require [aoc2021.day18 :as sut]
            [clojure.string :as str]
            [clojure.test :as t]))

(def input-file "input/18")

(defn test-input []
  (map read-string
       (str/split (slurp input-file) #"\n")))

(t/deftest part-1-test
  (t/is (= 3816
           (sut/magnitude
            (reduce (fn [a b]
                      (sut/reducen [a b]))
                    (test-input))))))

(t/deftest part-2-test
  (let [nn (test-input)]
    (t/is (= 4819
             (apply max (mapcat (fn [[i j]]
                                  (let [a (nth nn i)
                                        b (nth nn j)]
                                    [(sut/magnitude (sut/reducen [a b]))
                                     (sut/magnitude (sut/reducen [b a]))]))
                                (sut/permute (count nn))))))))
