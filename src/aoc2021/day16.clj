(ns aoc2021.day16
  (:require [clojure.string :as str]))

;; Day 16: Decoding

(defn int4->bits [c]
  (let [n (- c (if (< c 58) 48 55))]
    (map #(bit-and 1 (bit-shift-right n %))
         (reverse (range 4)))))

(defn nibbles->int [s]
  (apply + (map (fn [v n]
                  (bit-shift-left v (* 4 n)))
                s
                (reverse (range (count s))))))


(defn bits->int [s]
  (apply + (map bit-shift-left
                s
                (reverse (range (count s))))))

(defn hex->bits [s]
  (mapcat (comp int4->bits int) (seq s)))

(defn read-int [s n]
  [(bits->int (take n s))
   (drop n s)])

(defn read-uint4 [s]
  (let [[end-marker s] (read-int s 1)
        [int4 s] (read-int s 4)]
    [{:done (= 0 end-marker)
      :value int4}
     s]))

(defn read-literal [s]
  (loop [acc [] s s]
    (let [[{:keys [done value]} s] (read-uint4 s)
          acc (conj acc value)]
      (if done
        [(nibbles->int acc) s]
        (recur acc s)))))

(defn read-substream [s]
  (let [[substream-len s] (read-int s 15)
        substream (take substream-len s)
        s (drop substream-len s)]
    [(read-packets substream)
     s]))

(defn read-n-packets [s]
  (let [[packet-count s] (read-int s 11)]
    (loop [acc [] s s n packet-count]
      (if (= 0 n)
        [acc
         s]
        (let [[packet s] (read-packet s)]
          (recur (conj acc packet) s (dec n)))))))

(defn read-operator [s]
  (let [[length-type s] (read-int s 1)]
    (if (= 0 length-type)
      (read-substream s)
      (read-n-packets s))))

(defn read-header [s]
  (when (< 7 (count s))
    (let [[version s] (read-int s 3)
          [id s] (read-int s 3)]
      [{:version version :id id} s])))

(defn read-packet [s]
  (let [[header s] (read-header s)]
    (if-not header
      [nil s]
      (let [readfn (if (= 4 (:id header))
                     read-literal
                     read-operator)]
        (let [[value s] (readfn s)]
          [{:header header
            :value value}
           s])))))

(defn read-packets [s]
  (loop [acc [] s s]
    (let [[p s] (read-packet s)]
      (if-not p
        acc
        (recur (conj acc p) s)))))
