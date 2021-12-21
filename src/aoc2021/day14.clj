(ns aoc2021.day14
  (:require [clojure.string :as str]))

;; Day 14: Summation of exponential function outputs

(def input-file "input/14")

(defn parse-template [parser line]
  (assoc parser
         :frequencies (frequencies line)
         :template (frequencies (partition 2 1 line))
         :mode :blank))

(defn parse-blank [parser line]
  (assoc parser :mode :rules))

(def rule-re #"(\p{Upper})(\p{Upper}) -> (\p{Upper})")

(defn parse-rule [parser line]
  (let [[a b c] (map first (rest (re-matches rule-re line)))]
    (assoc-in parser [:rules [a b]] [[a c] [c b]])))

(defn parse-line [{:keys [mode] :as parser} line]
  (let [parser-fn ({:template parse-template
                    :blank parse-blank
                    :rules parse-rule} mode)]
    (parser-fn parser line)))

(defn parse-input [input]
  (reduce parse-line {:mode :template} input))

(defn load-input [f]
  (parse-input (str/split (slurp f) #"\n")))

(defn inc-in [m ks n]
  (update-in m ks #(+ (or % 0) n)))

(defn run-cycle [{:keys [template rules] :as state}]
  (reduce (fn [state [pair count]]
            (if-let [insertion (get rules pair)]
              (-> state
                  (inc-in [:template (first insertion)] count)
                  (inc-in [:template (second insertion)] count)
                  (inc-in [:frequencies (second (first insertion))] count))
              (inc-in state [:template pair] count)))
          (dissoc state :template)
          template))

(defn run-cycles [n f]
  (apply comp (take n (repeat f))))

(defn solve [n]
  (let [m (:frequencies ((run-cycles n run-cycle) (load-input input-file)))
        m' (sort-by first (map (fn [[k v]] [v k]) m))]
    (- (first (last m'))
       (first (first m')))))

(defn part-1 []
  (solve 10))

(defn part-2 []
  (solve 40))
