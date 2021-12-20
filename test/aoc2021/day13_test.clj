(ns aoc2021.day13-test
  (:require [aoc2021.day13 :as sut]
            [clojure.test :as t]))

(def flag      ; 'BLKJRBAG'
  "###  #    #  #   ## ###  ###   ##   ## \n#  # #    # #     # #  # #  # #  # #  #\n###  #    ##      # #  # ###  #  # #   \n#  # #    # #     # ###  #  # #### # ##\n#  # #    # #  #  # # #  #  # #  # #  #\n###  #### #  #  ##  #  # ###  #  #  ###")

(t/deftest day13-test
  (t/is (= 755 (sut/part-1)))
  (t/is (= flag (sut/part-2))))
