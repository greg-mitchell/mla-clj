(ns mla-clj.minimax-player
  (:import (clojure.lang IObj PersistentQueue))
  (:require [mla-clj.engine :as en]))

(defrecord DecisionTuple [state utility best-child])

(defn utility
  "+1 if player wins, -1 if player loses, 0 if it is a draw or depth is 0, or nil
   if the state is not terminal."
  [rules player heuristic state depth]
  ;; TODO: Implement for non-2-player games (i.e. vector representation)
  (if-let [losers (en/losers? rules state)]
    (reduce #(+ %1 (if (= player %2) -1 1)) 0 losers)
    (when (= 0 depth)
      (heuristic state))))

(defn min-decision
  [^DecisionTuple current-decision ^DecisionTuple child]
  (if (> (.utility current-decision) (.utility child))
    (DecisionTuple. (.state current-decision) (.utility child) (.state child))
    current-decision))

(defn max-decision
  [^DecisionTuple current-decision ^DecisionTuple child]
  (if (< (.utility current-decision) (.utility child))
    (DecisionTuple. (.state current-decision) (.utility child) (.state child))
    current-decision))

(defn- get-maximizing-fn
  [previous-fn]
  (if (= previous-fn min-decision)
    max-decision
    min-decision))

(defn- get-best-child-seed
  [state max-fn]
  (if (= max-fn min-decision)
    (DecisionTuple. state Integer/MAX_VALUE nil)
    (DecisionTuple. state Integer/MIN_VALUE nil)))

(declare minimax-decision)

(defn- ^DecisionTuple minimax-decision*
  "Returns a DecisionTuple of the successor to select.
   WARNING: consumes stack"
  [rules player utility-heuristic max-fn state depth]
  (if-let [u (utility rules player utility-heuristic state depth)]
    (DecisionTuple. state u nil)
    (let [successors (en/successors rules state)
          next-fn (get-maximizing-fn max-fn)
          seed (get-best-child-seed state max-fn)
          get-best-child (fn [best child]
                           (max-fn best (minimax-decision rules player utility-heuristic
                                                          next-fn child (dec depth))))]
      (reduce get-best-child seed successors))))

(def ^DecisionTuple minimax-decision
  (memoize minimax-decision*))

(defn get-move
  [{:keys [rules state]} player]
  ;; TODO: implement heuristics
  (let [move (minimax-decision rules player (constantly 0) max-decision state 16)]
    (.best_child move)))

(deftype MinimaxPlayer [_meta]
  en/Player
  (move [this game successors]
    (get-move game this))

  IObj
  (withMeta [this meta]
    (MinimaxPlayer. meta))
  (meta [this]
    _meta))
