(ns aoc2021.day20
  (:require [clojure.string :as str]))

(def ex-algo
"..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##
#..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###
.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#.
.#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#.....
.#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#..
...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.....
..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#")

(def ex-image
"#..#.
#....
##..#
..#..
..###")


(def ex (map seq (str/split ex-image #"\n")))

(def expand [i]
  (let [d (count i)]
    d) (repeat \.))))
(conj

(def image-value [image x y]
  (+ (if (= 0 x)
