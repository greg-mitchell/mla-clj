(ns mla-clj.naughts-and-crosses
  (:import (clojure.lang IObj))
  (:require
    [clojure.string :as s]
    [mla-clj.engine :as en]
    [mla-clj.util :as util]))

(defn- get-piece [player]
  (::piece (meta player)))

(deftype TTTPlayer [_meta]
  en/Player
  (move [this game successors]
    (rand-nth successors))

  IObj
  (withMeta [this meta]
    (TTTPlayer. meta))
  (meta [this]
    _meta)

  Object
  (toString [this]
    (str (get-piece this))))

(defn create-player []
  (TTTPlayer. nil))

(defn get-initial
  [players]
  {:pre [(= 2 (count players))]}
  (let [x-player (with-meta (first players) {::piece :X})
        o-player (with-meta (second players) {::piece :O})]
    (en/->State 0 [x-player o-player] (vec (repeat 9 :_)) nil)))

(defn get-successors [state]
  (let [curr-state (:state state)
        ap (get-piece (first (:players state)))
        place (fn [placement]
                (en/map->State {:state   (assoc curr-state placement ap)
                                :players (util/rotate-left (:players state))
                                :turn    (inc (:turn state))
                                :parent  nil                ;; TODO: add parent link
                                }))
        placements (->> (map vector (range) curr-state)
                        (filter #(= :_ (second %)))
                        (map first))]
    (map place placements)))

(defn- select-loser
  [marker players]
  (when-not (= :_ marker)
    (remove #(= marker (get-piece %)) players)))

(defn- equal-at
  [state & indices]
  (apply = (map (partial nth state) indices)))

(defmacro ^:private check-loss-at
  [players state & index-tuples]
  (let [predicates (map (partial concat `(equal-at ~state)) index-tuples)
        bodies (map #(list 'select-loser (list 'nth state (first %)) players) index-tuples)]
    `(cond
       ~@(interleave predicates bodies)
       :else nil)))

(defn get-losers [{:keys [players state]}]
  (if (every? (partial not= :_) state)
    players
    (check-loss-at players state
      [0 1 2]
      [3 4 5]
      [6 7 8]
      [0 3 6]
      [1 4 7]
      [2 5 8]
      [0 4 8]
      [2 4 6])))

(defn get-str [state]
  (let [ap (first (:players state))
        marks (map name (:state state))
        grid (partition 3 marks)
        rows (map (partial s/join " ") grid)]
    (str ap " to move:\n\t" (s/join "\n\t" rows) "\n")))

(deftype NaughtsAndCrosses []
  en/GameRules
  (initial [this players]
    (get-initial players))
  (successors [this state]
    (get-successors state))
  (losers? [this state]
    (get-losers state))
  (str-state [this state]
    (get-str state))
  Object
  (toString [this]
    ""))