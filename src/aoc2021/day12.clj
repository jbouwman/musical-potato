(ns aoc2021.day12
  (:require [clojure.string :as str]))

;; Day 11: Graph search
;;
;; Generate and test over undirected cyclic graph

(def input-file "input/12")

(defn line->edge [l]
  (str/split l #"-"))

(defn read-graph [f]
  (with-open [reader (clojure.java.io/reader f)]
    (doall (map line->edge (line-seq reader)))))

(defn cave-map [s]
  (reduce (fn [m [a b]]
            (-> m
                (update a conj b)
                (update b conj a)))
          {} s))

(defn big? [cave]
  (<= 65 (int (first cave)) 91))

(defn visit [state cave]
  (-> state
      (update :path conj cave)
      (update-in [:visit-count cave] #(inc (or % 0)))))

(defn visit-count [state cave]
  (get-in state [:visit-count cave] 0))

(defn cave-paths [state can-visit-fn cave-map]
  (reduce (fn [paths cave]
            (concat
             paths
             (if (= cave "end")
               (list (reverse (:path (visit state cave))))
               (cave-paths (visit state cave) can-visit-fn cave-map))))
          nil
          (filter (partial can-visit-fn state)
                  (get cave-map (first (:path state))))))

(defn path-count [can-visit?]
  (count (cave-paths (visit {} "start")
                     can-visit?
                     (cave-map (read-graph input-file)))))

(defn part-1 []
  (path-count
   (fn [state cave]
     (or (big? cave)
         (and (not (= cave "start"))
              (= (visit-count state cave) 0))))))

(defn has-visited-small-cave-twice? [{:keys [visit-count]}]
  (some (fn [[cave visit-count]]
          (and (not (big? cave))
               (< 1 visit-count)))
        visit-count))

(defn part-2 []
  (path-count
   (fn [state cave]
     (or (big? cave)
         (and (not (= cave "start"))
              (<= (visit-count state cave)
                  (if (has-visited-small-cave-twice? state)
                    0 1)))))))
