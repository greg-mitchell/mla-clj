(ns mla-clj.naughts-and-crosses-test
  (:require [mla-clj.naughts-and-crosses :refer :all]
            [mla-clj.engine :as en]
            [clojure.test :refer :all]))

(deftest test-get-losers
  (testing "First row"
    (is (= [:O]
           (get-losers (en/->State 0 [:O :X] [:X :X :X :O :_ :O :_ :_ :_] nil)))))
  (testing "Second row"
    (is (= [:X]
           (get-losers (en/->State 0 [:X :O] [:X :_ :X :O :O :O :X :_ :_] nil)))))
  (testing "Third row"
    (is (= [:O]
           (get-losers (en/->State 0 [:O :X] [:X :_ :O :O :_ :O :X :X :X] nil)))))
  (testing "First column"
    (is (= [:O]
           (get-losers (en/->State 0 [:O :X] [:X :O :_ :X :_ :O :X :_ :_] nil)))))
  (testing "Second column"
    (is (= [:O]
           (get-losers (en/->State 0 [:O :X] [:O :X :_ :O :X :_ :_ :X :_] nil)))))
  (testing "Third column"
    (is (= [:O]
           (get-losers (en/->State 0 [:O :X] [:_ :O :X :O :_ :X :_ :_ :X] nil)))))
  (testing "Diagonal down-right"
    (is (= [:O]
           (get-losers (en/->State 0 [:O :X] [:X :O :_ :O :X :_ :_ :_ :X] nil)))))
  (testing "Diagonal up-right"
    (is (= [:X]
           (get-losers (en/->State 0 [:X :O] [:X :_ :O :X :O :_ :O :_ :_] nil)))))
  (testing "Draw"
    (is (= [:X :O]
           (get-losers (en/->State 0 [:X :O] [:X :O :O :X :O :X :O :X :O] nil)))))
  (testing "No losers"
    (is (nil? (get-losers (en/->State 0 [:X :O] [:X :_ :O :O :_ :O :X :_ :X] nil))))))