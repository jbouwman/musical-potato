(ns aoc2021.day19
  (:require [clojure.string :as str]))

;; Day 14: geometric matching

;; orientation determined by permutation of signs

(def input-file "input/19")

(def probe-re #"^(-?\p{Digit}+),(-?\p{Digit}+),(-?\p{Digit}+)$")

(def scanner-re #"^--- scanner (-?\p{Digit}+) ---$")

(def parser
  {:state :scanner :result []})

(defn parse-probe [line]
  (vec (map #(Integer/parseInt %)
            (rest (re-matches probe-re line)))))

(defn parse-scanner [line]
  (Integer/parseInt (second (re-matches scanner-re line))))

(defn parse-line [parser line]
  (case (:state parser)
    :scanner (let [scanner (parse-scanner line)]
               (assoc parser
                      :current {:scanner scanner
                                :probes []}
                      :state :probe))
    :probe (if (= 0 (count line))
             (-> parser
                 (update :result conj (:current parser))
                 (dissoc :current)
                 (assoc :state :scanner))
             (let [probe (parse-probe line)]
               (update-in parser [:current :probes] conj probe)))))

(defn close-parser [{:keys [current] :as parser}]
  (cond-> parser
    (:probes current)
    (update :result conj current)))

(defn parse-input [lines]
  (:result (close-parser (reduce parse-line parser lines))))

(defn read-input [f]
  (parse-input (str/split (slurp f) #"\n")))

(def ex (read-input "input/17ex"))

(def p1 (:probes (first ex)))

(def p2 (:probes (second ex)))

(def p1min (apply min (map first p1)))

(map #(- % p1min)
     (map first p1))
