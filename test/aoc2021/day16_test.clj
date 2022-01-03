(ns aoc2021.day16-test
  (:require [aoc2021.day16 :as sut]
            [clojure.test :as t]))


(def input-file "input/16")

(def ex-1 "D2FE28")

(t/deftest day16-ex1-test
  (t/is (= [{:header {:version 6, :id 4}, :value 2021}]
           (sut/read-packet (sut/hex->bits ex-1)))))

(def ex-2 "38006F45291200")

(t/deftest day16-ex2-test
  (t/is (= [{:header {:version 1, :id 6},
             :value
             {:packets
              [{:header {:version 6, :id 4}, :value 10}
               {:header {:version 2, :id 4}, :value 20}]}}]
           (sut/read-packets (sut/hex->bits ex-2)))))

(def ex-3 "EE00D40C823060")

(t/deftest day16-ex3-test
  (t/is (= [{:header {:version 7, :id 3},
             :value
             {:packets
              [{:header {:version 2, :id 4}, :value 1}
               {:header {:version 4, :id 4}, :value 2}
               {:header {:version 1, :id 4}, :value 3}]}}]
           (sut/read-packets (sut/hex->bits ex-3)))))

;; represents an operator packet (version 4) which contains an operator
;; packet (version 1) which contains an operator packet (version 5) which
;; contains a literal value (version 6); this packet has a version sum of 16.

(sut/read-packets (sut/hex->bits "8A004A801A8002F478"))

;; represents an operator packet (version 3) which contains two sub-packets;
;; each sub-packet is an operator packet that contains two literal values. This
;; packet has a version sum of 12.

(sut/read-packet (sut/hex->bits "620080001611562C8802118E34"))

;; has the same structure as the previous example, but the outermost packet uses
;; a different length type ID. This packet has a version sum of 23.

(sut/read-packets (sut/hex->bits "C0015000016115A2E0802F182340"))

;; is an operator packet that contains an operator packet that contains an
;; operator packet that contains five literal values; it has a version sum of
;; 31.

(def x (sut/read-packets (sut/hex->bits "A0016C880162017C3686B18A3D4780")))

(defn version-sum [{:keys [header value] :as packet}]
  (let [{:keys [version id]} header]
    (if (= 4 id)
      version
      (+ version (apply + (map version-sum value))))))


(version-sum (first x))

(version-sum (first (sut/read-packets (sut/hex->bits (slurp input-file))))) ; works
