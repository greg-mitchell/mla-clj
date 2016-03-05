(ns mla-clj.ab-player
  (:require [mla-clj.engine :as en]
            [mla-clj.minimax-player :as mmp])
  (:import [clojure.lang IObj PersistentVector]
           [mla_clj.minimax_player DecisionTuple]))

;; todo: delete
(defn ab-decision
  "Performs a depth-limited search using α-β pruning. Returns a DecisionTuple.
   Warning: Consumes stack!"
  [rules player utility-heuristic state depth a b maximizing]
  {:post [(:state %)]}
  (if-let [u (mmp/utility rules player utility-heuristic state depth)]
    ;; TODO: This is obviously wrong. Need to save state or child somewhere
    (mmp/->DecisionTuple state u state)
    ;; TODO: Sort successors (perhaps by previous iterations in iterative deepening)
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
              seed (mmp/->DecisionTuple state Integer/MIN_VALUE state)]
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

;; GameTree : [value state children]
;; value : -1 -> +1 where -1 is a loss for calling player and +1 is a win
;; state : State
;; best-child : GameTree | e

(defrecord GameTree [value state best-child])

(declare min-value)
(declare max-value)

(defn max-value* [rules player utility-heuristic depth alpha beta tree]
  "Inputs:
   * depth - remaining depth from bound down to 0
   * player- player referrent for maximizing utility.
   * alpha - the best alternative for MAX along the path to tree
   * beta  - the best alternative for MIN along the path to tree
   * tree  - the current state in the game tree
  Returns:
   * a GameTree with its utility value. Utility is calculated with respect to player always."
  (if-let [u (mmp/utility rules player utility-heuristic (:state tree) depth)]
    ;; base case: terminal (or depth-bounded) state, set the value of the node
    (do
      (println "Found terminal state" (en/str-state rules (:state tree)) "with utility" u)
      (assoc tree :value u)
      )
    ;; recursive case: non-terminal state, recurse into each child
    (let [children (->> (en/successors rules (:state tree))
                        ;; advance state
                        (map (fn [child] (->GameTree Integer/MIN_VALUE child nil))))]
      ;; depth-first search of the tree
      (loop [c (first children)
             crest (rest children)
             a alpha
             b beta
             u Integer/MIN_VALUE]
        (let [child-min (min-value rules player utility-heuristic (dec depth) alpha beta c)
              best-u (max u (:value child-min))]
          (cond
            ;; base cases:
            ;; terminate search because min will never choose this node
            (or (>= best-u b)
                ;; no more children, this node's value is the max of its children
                (empty? crest))
            (assoc tree :value best-u :best-child child-min)

            ;; recursive case: examine more children to find the best max child
            :else (recur (first crest) (rest crest) a b best-u)))))))

(defn min-value* [rules player utility-heuristic depth alpha beta tree]
  "Inputs:
   * depth - remaining depth from bound down to 0
   * player- player referrent for minimizing utility.
   * alpha - the best alternative for MAX along the path to tree
   * beta  - the best alternative for MIN along the path to tree
   * tree  - the current state in the game tree
  Returns:
   * the utility value of the state. Utility is calculated with respect to player always."
  (if-let [u (mmp/utility rules player utility-heuristic (:state tree) depth)]
    ;; base case: terminal (or depth-bounded) state, set the value of the node
    (do
      (println "Found terminal state" (en/str-state rules (:state tree)) "with utility" u)
      (assoc tree :value u))
    ;; recursive case: non-terminal state, recurse into each child
    (let [children (->> (en/successors rules (:state tree))
                        (map (fn [child] (->GameTree Integer/MAX_VALUE child nil))))]
      ;; depth-first search of the tree
      (loop [c (first children)
             crest (rest children)
             a alpha
             b beta
             u Integer/MAX_VALUE]
        (let [child-max (max-value rules player utility-heuristic (dec depth) alpha beta c)
              best-u (min u (:value child-max))]
          (cond
            ;; base cases:
                ;; terminate search because max will never choose this node
            (or (<= best-u a)
                ;; no more children, this node's value is the min of its children
                (empty? crest))
            (assoc tree :value best-u :best-child child-max)

            ;; recursive case: examine more children to find the best min child
            :else (recur (first crest) (rest crest) a b best-u)))))))

(def min-value
  (memoize min-value*))

(def max-value
  (memoize max-value*))

(defn get-move
  [{:keys [rules _players state] :as _game} player]
  (println "Finding best move for state" (en/str-state rules state))
  (let [utility-heuristic (fn [& args] (println "Using heuristic") 0)
        depth 16
        tree (->GameTree Integer/MIN_VALUE state nil)
        updated-tree (max-value rules player utility-heuristic depth Integer/MIN_VALUE Integer/MAX_VALUE tree)]
    (println "Found value of state: " (:value updated-tree))
    (-> updated-tree :best-child :state)))

(deftype AlphaBetaPlayer [_meta]
  en/Player
  (move [this game successors]
    (get-move game this))

  IObj
  (withMeta [this meta]
    (AlphaBetaPlayer. meta))
  (meta [this]
    _meta))

(defn player []
  (AlphaBetaPlayer. {}))
