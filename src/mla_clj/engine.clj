(ns mla-clj.engine
  )

(defrecord Game [rules players state])

(defrecord State [turn players state parent])

(defprotocol GameRules
  "Protocol encapsulating the rules of a game. Assumptions for games:
    * N-player
    * Zero-sum - a winner is by definition not a loser, and winning is a function
                 of the state. There can be multiple losers (draw).
    * Turn-based
    * Finite state - A state can be represented in finite memory.
    * Finite moves - There are finitely many moves per state."
  (initial [this players]
           "Generate an initial state given a list of players in player order
            (first to start). GameRules may add metadata to the players for
            their own use.")
  (successors [this ^State state]
              "Return a seq of successors to this state.")
  (losers? [this ^State state]
           "The subset of players who are losers in the state, or nil if there
            are no losers.")
  (str-state [this ^State state]
             "Return a string representation of the state suitable for printing."))

(defprotocol Player
  "Protocol defining a player in the game. Players should also implement IObj."
  (move [this game successors]
        "Select a successor. No resource limitations on selecting a move."))

(defn create-game
  "Creates a game in the initial state for the given rules and players."
  [game-rules players]
  (let [state (initial game-rules players)
        initialized-players (:players state)]
    (Game. game-rules initialized-players state)))

(defn next-state
  "Moves the game to the next state by asking the active player for a move.
   TODO: Document terminal conditions"
  [{:keys [rules state] :as game}]
  (let [successors (successors rules state)
        ap (first (:players state))
        next-state (move ap game successors)
        losers (losers? rules next-state)]
    (println (str-state rules next-state))
    (if losers
      {:losers losers :state state}
      (assoc game :state next-state))))

(defn play-game
  "Plays a game to finish with the given rules and players. Returns a list of
   losing players."
  [game-rules players]
  (loop [game (create-game game-rules players)]
    (or (:losers game)
        (recur (next-state game)))))