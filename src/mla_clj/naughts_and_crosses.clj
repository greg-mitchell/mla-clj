(ns mla-clj.naughts-and-crosses
  (:require
    [clojure.string :as s]
    [clojure.math.numeric-tower :as m]
    [mla-clj.engine :as en]))

(defn conjugate [f g & [g-inv]]
  (comp (or g-inv g) f g))

(defn composite [f f-inv n x]
  (nth (iterate (if (pos? n) f f-inv) x) (m/abs n)))

(defn rotate-left [xs]
  (when (seq xs) (concat (rest xs) [(first xs)])))

(def rotate-right (conjugate rotate-left reverse))

(defn rotate [n xs]
  (composite rotate-left rotate-right n xs))

(defn get-initial [starting-player argmap]
  (en/->State 0 starting-player (vec (repeat 9 :_)) nil))

(defn get-successors [state]
  (let [curr-state (:state state)
        ap (first (:players state))
        place (fn [placement]
                (en/map->State {:state   (assoc curr-state placement ap)
                                :players (rotate-left (:players state))
                                :turn    (inc (:turn state))
                                :parent  state}))
        placements (->> (map vector (range) curr-state)
                        (filter #(= :_ (second %)))
                        (map first))]
    (map place placements)))

(defn get-losers [players state]
  ;; TODO
  nil
  )

(defn get-str [state]
  (let [ap (:active-player state)
        marks (map name (:state state))
        grid (partition 3 marks)
        rows (map (partial s/join " ") grid)]
    (str (:active-player state) " to move:\n\t"
         (s/join "\n\t" rows)
         "\n")))

(deftype NaughtsAndCrosses []
  en/GameRules
  (initial [this starting-player argmap]
    (get-initial starting-player argmap))
  (successors [this state]
    (get-successors state))
  (losers? [this players state]
    (get-losers players state))
  (str-state [this state]
    (get-str state)))