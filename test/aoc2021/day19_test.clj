(ns aoc2021.day19-test
  (:require [aoc2021.day19 :as sut]
            [clojure.string :as str]
            [clojure.test :as t]))

(def input-file "input/19")

(t/deftest parse-position-test
  (t/is (= [-640 638 699]
           (sut/parse-position "-640,638,699"))))

(t/deftest parse-scanner-test
  (t/is (= 1
           (sut/parse-scanner "--- scanner 1 ---"))))

(t/deftest parse-test
  (t/is (= {:state :probe
            :result []
            :current {:scanner 1
                      :probes []}}
           (sut/parse-line sut/parser
                           "--- scanner 1 ---")))

  (t/is (= [{:scanner 1
             :probes [[-640 638 699]
                      [-140 138 599]]}
            {:scanner 21
             :probes [[1 1 1]]}]
           (sut/parse-input ["--- scanner 1 ---"
                             "-640,638,699"
                             "-140,138,599"
                             ""
                             "--- scanner 21 ---"
                             "1,1,1"
                             ""]))))
