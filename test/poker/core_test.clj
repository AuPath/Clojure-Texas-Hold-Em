(ns poker.core-test
  (:require [clojure.test :refer :all]
            [poker.core :refer :all]))

(def hand-high-card (list (card 14 \C)
                          (card 13 \C)
                          (card 10 \C)
                          (card 9 \C)
                          (card 8 \F)))

(def hand-pair (list (card 14 \C)
                     (card 14 \C)
                     (card 12 \F)
                     (card 10 \C)
                     (card 9 \C)))

(def hand-two-pair (list (card 14 \C)
                          (card 14 \C)
                          (card 12 \F)
                          (card 12 \C)
                          (card 9 \C)))

(def hand-three-of-a-kind (list (card 14 \C)
                        (card 14 \C)
                        (card 14 \F)
                        (card 10 \C)
                        (card 9 \C)))

(def hand-poker (list (card 14 \C)
                         (card 14 \C)
                         (card 14 \F)
                         (card 14 \C)
                         (card 9 \C)))

(def hand-full (list (card 14 \C)
                        (card 14 \C)
                        (card 14 \F)
                        (card 10 \C)
                        (card 10 \C)))

(def hand-straight (list (card 14 \C)
                         (card 13 \C)
                         (card 12 \F)
                         (card 11 \C)
                         (card 10 \C)))

(def hand-straight-flush (list (card 11 \C)
                               (card 10 \C)
                               (card 9 \C)
                               (card 8 \C)
                               (card 7 \C)))

(def hand-royal-flush (list (card 14 \C)
                          (card 13 \C)
                          (card 12 \C)
                          (card 11 \C)
                          (card 10 \C)))

(def hand-flush (list (card 14 \C)
                          (card 9 \C)
                          (card 12 \C)
                          (card 11 \C)
                          (card 10 \C)))

(deftest hand-type-predicate-test
  (testing "Testing predicates used to determine a hand's type."
    (is (high-card? hand-high-card) "High card detection fails.")
    (is (pair? hand-pair) "Pair detection fails.")
    (is (two-pair? hand-two-pair) "Two-Pair detection fails.")
    (is (three-of-a-kind? hand-three-of-a-kind) "Three-Of-A-Kind detection fails.")
    (is (flush? hand-flush) "Flush detection fails.")
    (is (straight? hand-straight) "Straight detection fails.")
    (is (poker? hand-poker) "Poker detection fails.")
    (is (full? hand-full) "Full detection fails.")
    (is (straight-flush? hand-straight-flush) "Straight-Flush detection fails.")    
    (is (royal-flush? hand-royal-flush) "Royal-Flush detection fails.")))

(deftest inter-hand-ranking-test
  (testing "Testing ranking order between different hand types."
    (is (apply < (map hand-value (list hand-high-card
                                       hand-pair
                                       hand-two-pair
                                       hand-three-of-a-kind
                                       hand-straight
                                       hand-flush
                                       hand-full
                                       hand-poker
                                       hand-straight-flush
                                       hand-royal-flush))))))
