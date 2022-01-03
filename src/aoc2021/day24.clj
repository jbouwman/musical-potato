(ns aoc2021.day24
  (:require [clojure.string :as str]))

;; Day 24: ?

(def alu
  (zipmap [:w :x :y :z] (repeat 0)))

(defn sref [s]
  (if (contains? #{"w" "x" "y" "z"} s)
    (let [s' (keyword s)]
      (fn [alu]
        (get alu s')))
    (let [i (Integer/parseInt s)]
      (fn [_alu]
        i))))

(defn binary-opcode [[dst src] f]
  (let [dst (keyword dst)]
    (fn [alu]
      (assoc alu dst (f (dst alu) ((sref src) alu))))))

(defmulti parse-opcode (fn [op args] op))

(defn read-input [alu]
  (let [[first & rest] (:input alu)]
    (when-not first
      (throw (Exception. "Input exhausted")))
    [first (assoc alu :input rest)]))

(defmethod parse-opcode "inp" [_ args]
  (let [dst (keyword (first args))]
    (fn [alu]
      (let [[in alu] (read-input alu)]
        (assoc alu dst in)))))

(defmethod parse-opcode "mul" [_ args]
  (binary-opcode args *))

(defmethod parse-opcode "div" [_ args]
  (binary-opcode args quot))

(defmethod parse-opcode "add" [_ args]
  (binary-opcode args +))

(defmethod parse-opcode "mod" [_ args]
  (binary-opcode args mod))

(defmethod parse-opcode "eql" [_ args]
  (binary-opcode args (fn [x y]
                        (if (= x y) 1 0))))

(defn parse-instruction [s]
  (let [[op & args] (str/split s #" ")]
    (parse-opcode op args)))

(defn load-input [f]
  (map parse-instruction (str/split (slurp f) #"\n")))

(defn load-inst [i]
  (map parse-instruction i))

(defn load-string [s]
  (map parse-instruction (str/split s #"\n")))

(defn init [input]
  (assoc alu :input input))

(defn trace-prog [text input regs]
  (let [lines (str/split text #"\n")
        prog (map parse-instruction lines)
        alu (merge (init input) regs)
        fstate (reduce (fn [alu op]
                         (let [alu (op alu)]
                           (update alu :trace conj
                                   (select-keys alu [:w :x :y :z]))))
                       (assoc alu :trace [])
                       prog)]
    (map (fn [line mstate]
           (format "%-16s %10d %10d %10d %10d"
                   line
                   (:w mstate)
                   (:x mstate)
                   (:y mstate)
                   (:z mstate)))
         lines
         (conj (:trace fstate)
               (select-keys alu [:w :x :y :z])))))

(defn run-prog [alu prog]
  (reduce #(%2 %) alu prog))
