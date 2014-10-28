(ns mla-clj.ab-player
  (:require [mla-clj.engine :as en]
            [mla-clj.minimax-player :as mmp])
  (:import [clojure.lang IObj]))

(defn ab-decision
  "Performs a depth-limited search using α-β pruning. Returns a DecisionTuple.
   Warning: Consumes stack!"
  [rules player utility-heuristic state depth a b maximizing]
  (if-let [u (mmp/utility rules player utility-heuristic state depth)]
    (mmp/->DecisionTuple state u nil)
    (let [successors (en/successors rules state)
          recurse (partial ab-decision rules player utility-heuristic)]
      (if maximizing
        ;; maximizing fold
        (let [max-child (fn [max-decision child]
                          (let [child-decision (recurse
                                                 child
                                                 (dec depth)
                                                 (:utility max-decision)
                                                 b
                                                 (not maximizing))
                                max-decision (mmp/max-decision max-decision
                                                               child-decision)]
                            (if (<= b (:utility max-decision))
                              (reduced max-decision)
                              max-decision)))
              seed (mmp/->DecisionTuple state Integer/MIN_VALUE nil)]
          (reduce max-child seed successors))
        ;; minimizing fold
        (let [min-child (fn [min-decision child]
                          (let [child-decision (recurse
                                                 child
                                                 (dec depth)
                                                 a
                                                 (:utility min-decision)
                                                 (not maximizing))
                                min-decision (mmp/min-decision min-decision
                                                               child-decision)]
                            (if (<= b (:utility min-decision))
                              (reduced min-decision)
                              min-decision)))
              seed (mmp/->DecisionTuple state Integer/MIN_VALUE nil)]
          (reduce min-child seed successors))))))

(defn get-move
  [{:keys [rules state]} player]
  (let [move (ab-decision rules player (constantly 0) state 16 Integer/MIN_VALUE Integer/MAX_VALUE true)]
    (:best_child move)))

(deftype AlphaBetaPlayer [_meta]
  en/Player
  (move [this game successors]
    (get-move game this))

  IObj
  (withMeta [this meta]
    (AlphaBetaPlayer. meta))
  (meta [this]
    _meta))
