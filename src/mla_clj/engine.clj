(ns mla-clj.engine
  )

(defrecord Game [rules players state])

(defrecord State [turn players state parent])

(defprotocol GameRules
  "Protocol encapsulating the rules of a game. Assumptions for games:
    * N-player
    * Zero-sum - a winner is by definition not a loser,
                 and winning is a function of the state.
    * Turn-based
    * Finite state - A state can be represented in finite memory.
    * Finite moves - There are finitely many moves per state."
  (initial [this starting-player argmap] "Generate an initial state given a
                                          starting player and an argument map.")
  (successors [this state] "Return a seq of successors to this state.")
  (losers? [this players state] "The subset of players who are losers in the
                                 state, or nil if there are no losers.")
  (str-state [this state] "Return a string representation of the state suitable
                           for printing."))

(defprotocol Player
  "Protocol defining a player in the game."
  (move [this game moves] "Select a move to make from moves. No resource
                           limitations on selecting a move."))

(defn create-game
  "Creates a game in the initial state for the given rules and players"
  [game-rules players]
  (Game. game-rules players (initial game-rules (first players) nil)))

(defn next-state
  "Moves the game to the next state by asking the active player for a move.
   TODO: Document terminal conditions"
  [game]
  ;; TODO
  )