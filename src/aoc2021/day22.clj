(ns aoc2021.day22
  (:require [clojure.string :as str]))

(def input "input/22")

(def command-re #"(.*) x=(-?\p{Digit}+)\.\.(-?\p{Digit}+),y=(-?\p{Digit}+)\.\.(-?\p{Digit}+),z=(-?\p{Digit}+)\.\.(-?\p{Digit}+)")

(defn parse-command [line]
  (let [[state x1 x2 y1 y2 z1 z2]
        (rest (re-matches command-re line))]
    {:state (if (= state "off") :off :on)
     :region [[(Integer/parseInt x1) (Integer/parseInt x2)]
              [(Integer/parseInt y1) (Integer/parseInt y2)]
              [(Integer/parseInt z1) (Integer/parseInt z2)]]}))

(parse-command "on x=10..12,y=10..12,z=10..12")

(defn parse-input [s]
  (map parse-command (str/split s #"\n")))

(defn read-input [f]
  (map parse-command (str/split (slurp f) #"\n")))

(defn range-intersection [[r1a r1b] [r2a r2b]]
  (if (< r2a r1a)
    (range-intersection [r2a r2b] [r1a r1b])
    (cond (<= r1a r2a r1b)
          [(max r2a r1a) (min r2b r1b)])))

(defn region-intersection [[r1x r1y r1z]
                           [r2x r2y r2z]]
  (let [xi (range-intersection r1x r2x)
        yi (range-intersection r1y r2y)
        zi (range-intersection r1z r2z)]
    (when (and xi yi zi)
      [xi yi zi])))

(defn region-obscures [a b]
  (= b (region-intersection a b)))

(defn sort1 [[r1a r1b] [r2a r2b]]
  (if (< r2a r1a)
    [[r2a r2b] [r1a r1b]]
    [[r1a r1b] [r2a r2b]]))

(defn sub1 [[x1a x1b] [x2a x2b]]
  (cond (and (< x1a x2a x1b)
             (< x1a x2b x1b))
        [[x1a (dec x2a)]
         [(inc x2b) x1b]]
        (< x1b x2b)
        [[x1a (dec x2a)]]
        :else
        [[(inc x2b) x1b]]))

;; (sub1 [5 15] [10 20])
;; (sub1 [10 20] [5 15])

(defn region-xa [[rx ry rz]]
  (first rx))

(defn region-xb [[rx ry rz]]
  (second rx))

(defn region-ya [[rx ry rz]]
  (first ry))

(defn region-yb [[rx ry rz]]
  (second ry))

(defn region-za [[rx ry rz]]
  (first rz))

(defn region-zb [[rx ry rz]]
  (second rz))

(defn sub3 [[r1x r1y r1z] [r2x r2y r2z]]
  (let [has-s1? (< (first r1x)
                   (first r2x))
        has-s2? (< (first r1z)
                   (first r2z))
        has-s3? (< (second r2z)
                   (second r1z))
        has-s4? (< (first r1y)
                   (first r2y))
        has-s5? (< (second r2y)
                   (second r1y))
        has-s6? (< (second r2x)
                   (second r1x))]
    (cond-> []
      has-s1?
      (conj [[(first r1x) (dec (first r2x))]
            r1y r1z])
      has-s2?
      (conj [r2x r1y
            [(first r1z) (dec (first r2z))]])
      has-s3?
      (conj [r2x r1y
            [(inc (second r2z)) (second r1z)]])
      has-s4?
      (conj [r2x
             [(first r1y) (dec (first r2y))]
             [(first r1z) (dec (first r2z))]])
      has-s5?
      (conj [r2x
            [(inc (second r2y)) (second r1y)]
            [(inc (second r2z)) (second r1z)]])
 ;     has-s6?
  ;    (conj [[(inc (second r2x)) (second r1x)]
   ;          r1y r1z])
      )))

(sub3 [[11 12] [10 13] [10 10]]
      [[9 11] [9 11] [9 11]])

 (range-invalid? rx)

(defn add3 [[r1x r1y r1z] [r2x r2y r2z]]
  (let [has-s1? (< (first r2x)
                   (first r1x))
        has-s2? (< (first r2z)
                   (first r1z))
        has-s3? (< (second r1z)
                   (second r2z))
        has-s4? (< (first r2y)
                   (first r1y))
        has-s5? (< (second r1y)
                   (second r2y))
        has-s6? (< (second r1x)
                   (second r2x))]
    (cond-> [[r1x r1y r1z]]
      has-s1?
      (conj [[(first r2x) (dec (first r1x))]
             r2y r2z])
      has-s2?
      (conj [[(max (first r1x) (first r2x))
              (min (second r1x) (second r2x))]
             [(min (first r1y) (first r2y))
              (max (second r1y) (second r2y))]
             [(first r2z) (dec (first r1z))]])
      has-s3?
      (conj [[(max (first r1x) (first r2x))
             (min (second r1x) (second r2x))]
            [(min (first r1y) (first r2y))
             (max (second r1y) (second r2y))]
            [(inc (second r1z)) (second r2z)]])
      has-s4?
      (conj [[(max (first r1x) (first r2x))
             (min (second r1x) (second r2x))]
            [(first r2y) (dec (first r1y))]
            [(max (first r1z) (first r2z))
             (min (second r1z) (second r2z))]])
     has-s5?
     (conj [[(max (first r1x) (first r2x))
             (min (second r1x) (second r2x))]
            [(inc (second r1y)) (second r2y)]
            [(max (first r1z) (first r2z))
             (min (second r1z) (second r2z))]])
     has-s6?
     (conj [[(inc (second r1x)) (second r2x)]
            r2y r2z]))))




;; partion by reciprocal intersection

(range-intersection [5 8] [8 12])

(range-intersection [3 6] [1 4])

(region-obscures [[3 6] [4 7] [5 8]]
                 [[3 4] [4 5] [5 9]])

(def reactor-1 [[-50 50] [-50 50] [-50 50]])

(def ex-commands
  "on x=10..12,y=10..12,z=10..12
on x=11..13,y=11..13,z=11..13
off x=9..11,y=9..11,z=9..11
on x=10..10,y=10..10,z=10..10")

(defn regions-intersect? [r1 r2]
  (if (region-intersection r1 r2)
    :intersecting
    :disjoint))

(defn add-region [regions region]
  (let [{:keys [intersecting disjoint]}
        (group-by (partial regions-intersect? region) regions)]
    (if-not (empty? intersecting)
      (if (= 1 (count intersecting))
        (concat disjoint
                (add3 region (first intersecting)))
        (throw (ex-info "Multiple additive intersecting regions" {:regions regions
                                                         :intersecting intersecting
                                                         :region region})))
      (conj regions region))))

(defn range-invalid? [[r1 r2]]
  (< r2 r1))

(defn region-invalid? [[rx ry rz]]
  (or (range-invalid? rx)
      (range-invalid? ry)
       (range-invalid? rz)))

(defn sub-region [regions region]
  (let [{:keys [intersecting disjoint]}
        (group-by (partial regions-intersect? region) regions)]
    (reduce (fn [regions sregion]
              (let [result (sub3 sregion region)]
                (when (some region-invalid? result)
                  (throw (ex-info "oops" {:a sregion :b region})))
                (concat regions result)))
            disjoint
            intersecting)))

(defn process-command [regions command]
  (case (:state command)
    :on (add-region regions (:region command))
    :off (sub-region regions (:region command))
  ))

(defn range-size [[r1 r2]]
  (- (inc r2) r1))

(defn region-size [[rx ry rz]]
  (* (range-size rx)
     (range-size ry)
     (range-size rz)))


(def xx (reduce process-command [] (take 2 (parse-input ex-commands))))

xx

;; => ([[11 13] [11 13] [11 13]]
;;     [[10 10] [10 12] [10 12]]
;;     [[11 12] [10 13] [10 10]]
;;     [[11 12] [10 10] [11 12]])



(def oof [[9 11] [9 11] [9 11]])

(sub-region xx oof)


(nth (parse-input ex-commands) 2)

;; => {:state :off, :region [[9 11] [9 11] [9 11]]}






(def valid-cmds
  (filter (fn [command]
            (region-intersection (:region command) reactor-1))
          (read-input input)))

(defn range-contains-point? [[ra rb] p]
  (<= ra p rb))

(* 200000 200000 20000)

(defn region-contains-point? [[rx ry rz] [x y z]]
  (and (range-contains-point? rx x)
       (range-contains-point? ry y)
       (range-contains-point? rz z)))

(region-contains-point? [[50 55] [50 55] [50 54]] [51 41 51])


(defn point-on [p]
  (= :on (some (fn [command]
                 (when (region-contains-point? (:region command) p)
                   (:state command)))
               (reverse valid-cmds))))

(defn range-range [[r1 r2]]
  (range r1 (inc r2)))

(defn region-point-seq [[rx ry rz]]
  (mapcat (fn [z]
            (mapcat (fn [y]
                      (map (fn [x]
                             [x y z])
                           (range-range rx)))
                    (range-range ry)))
        (range-range rz)))

(reduce (fn [n p]
          (if (point-on p)
            (inc n) n))
        0
        (region-point-seq [[-100 100] [-100 100] [-100 100]]))

(point-on [3 3 33])

(take 20 (region-point-seq [[-50 50][-50 50][-50 50]]))

1523 low

              (point-on [0 0 0])
