(ns mla-clj.util
  (:require [clojure.math.numeric-tower :as m]))

(defn conjugate [f g & [g-inv]]
  (comp (or g-inv g) f g))

(defn composite [f f-inv n x]
  (nth (iterate (if (pos? n) f f-inv) x) (m/abs n)))

(defn rotate-left [xs]
  (when (seq xs) (concat (rest xs) [(first xs)])))

(def rotate-right (conjugate rotate-left reverse))

(defn rotate [n xs]
  (composite rotate-left rotate-right n xs))