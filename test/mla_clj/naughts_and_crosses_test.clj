(ns mla-clj.naughts-and-crosses-test
  (:require [mla-clj.naughts-and-crosses :refer :all]
            [mla-clj.engine :as en]
            [clojure.test :refer :all]))

(defn loser [markers state]
  (->> (en/->State 0 (map player markers) state nil)
       get-losers
       (map get-piece)))

(deftest test-get-losers
  #_(testing
    (testing "First row"
      (is (= [:O]
             (loser [:O :X] [:X :X :X :O :_ :O :_ :_ :_]))))
    (testing "Second row"
      (is (= [:X]
             (loser [:X :O] [:X :_ :X :O :O :O :X :_ :_]))))
    (testing "Third row"
      (is (= [:O]
             (loser [:O :X] [:X :_ :O :O :_ :O :X :X :X]))))
    (testing "First column"
      (is (= [:O]
             (loser [:O :X] [:X :O :_ :X :_ :O :X :_ :_]))))
    (testing "Second column"
      (is (= [:O]
             (loser [:O :X] [:O :X :_ :O :X :_ :_ :X :_]))))
    (testing "Third column"
      (is (= [:O]
             (loser [:O :X] [:_ :O :X :O :_ :X :_ :_ :X]))))
    (testing "Diagonal down-right"
      (is (= [:O]
             (loser [:O :X] [:X :O :_ :O :X :_ :_ :_ :X]))))
    (testing "Diagonal up-right"
      (is (= [:X]
             (loser [:X :O] [:X :_ :O :X :O :_ :O :_ :_]))))

    (testing "No losers"
      (is (empty? (loser [:X :O] [:X :_ :O :O :_ :O :X :_ :X]))))
    testing "Weird cases"
    (is (= [:O]
           (loser [:O :X] [:O :O :X :X :X :X :O :O :X])))
    (is (= [:O]
           (loser [:O :X] [:O :O :X :X :O :O :X :X :X]))))
  (testing
    (testing "Draw"
      (is (= [:X :O]
             (loser [:X :O] [:X :O :O :X :O :X :O :X :O]))))))
