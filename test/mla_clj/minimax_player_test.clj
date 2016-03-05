(ns mla-clj.minimax-player-test
  (:require [mla-clj.engine :as en]
            [mla-clj.minimax-player :as mmp]
            [mla-clj.naughts-and-crosses :as ttt]
            [clojure.test :refer :all]))

(deftest test-draw-tic-tac-toe
  (let [players (repeatedly 2 mmp/player)
        rules (ttt/rules)]
    (is (= 2 (count (en/play-game rules players))) "Perfect play should always result in a draw.")))
