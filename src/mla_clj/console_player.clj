(ns mla-clj.console-player
  (:import (clojure.lang IObj)
           (javax.swing JOptionPane))
  (:require [mla-clj.engine :as en]))

(defn print-successors
  [rules successors]
  (println "Choose a successor state:")
  (doseq [[i s] (map vector (range) successors)]
    (println (str " " (inc i) ".\t" (en/str-state rules s)))))

(defn- get-input
  "Workaround for Cursive not supporting read-line"
  [^String prompt]
  (JOptionPane/showInputDialog prompt))

(defn- prompt-for-selection
  [rules successors]
  (loop [l (get-input "Move?")]
    (let [move-num (#(try (Integer/parseInt %) (catch Exception ex)) l)]
      (if (and move-num (< 0 move-num (inc (count successors))))
        (dec move-num)
        (recur (get-input "Move?"))))))

(defn get-move
  [rules successors]
  (print-successors rules successors)
  (nth successors (prompt-for-selection rules successors)))

(deftype ConsolePlayer [_meta]
  en/Player
  (move [this game successors]
    (get-move (:rules game) successors))

  IObj
  (withMeta [this meta]
    (ConsolePlayer. meta))
  (meta [this]
    _meta))
