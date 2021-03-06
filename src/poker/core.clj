(ns poker.core
  (:gen-class)
  (:require [clojure.math.combinatorics :as combo]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;;; A,K,Q,J,T,9,8,7,6,5
;;; 14,13,12,11,10,9,8,7,6,5,4,3,2
;;; C clubs, D diamonds, H hearts, S spades
;;; R,B

(defn card
  "Returns a card with VALUE, SUIT."
  ([value suit] {:value value, :suit suit})
  ([vs] (let [[v s] vs
              x (str v)
              y (case x
                  "A" 14
                  "K" 13
                  "Q" 12
                  "J" 11
                  "T" 10
                  (Integer/parseInt x))]
          (card y s))))

(defn group-of-n?
  [n hand]
  (boolean
   (some #{n}
         (vals (frequencies (map :value hand))))))

(defn pair?
  [hand]
  (group-of-n? 2 hand))

(defn two-pair?
  [hand]
  (= 2
     (count (filter #(= 2 %)
                    (vals (frequencies (map :value hand)))))))

(defn three-of-a-kind?
  [hand]
  (group-of-n? 3 hand))

(defn straight?
  [hand]
  (let [values (sort (map :value hand))]
    (= values
       (range (apply min values) (+ 1 (apply max values))))))

(defn flush?
  [hand]
  (= 1 (count (group-by :suit hand))))

(defn full?
  [hand]
  (and (three-of-a-kind? hand)
       (pair? hand)))

(defn poker?
  [hand]
  (group-of-n? 4 hand))

(defn straight-flush?
  [hand]
  (and (straight? hand)
       (flush? hand)))

(defn royal-flush?
  [hand]
  (straight-flush? hand))

(defn high-card?
  [hand]
  (not
   ((some-fn pair? two-pair? three-of-a-kind?
             poker? straight? royal-flush? full? straight-flush?) hand)))

(defn update-game-pot
  "Update pot by AMOUNT"
  [game amount]
  (update game :pot #(+ % amount)))

(defn update-game-deck
  "Return new gamestate with DECK."
  [game deck]
  (assoc game :deck deck))

(defn player-cards
  "Gives CARDS to PLAYER."
  [player cards]
  (update player :hand #(concat % cards)))

(defn player
  "Returns player with PLAYER-ID from GAME."
  [game player-id]
  (get (:players game) player-id))

(defn player-money
  "Adds AMOUNT to PLAYER money."
  [player amount]
  (update player :money #(+ % amount)))

(defn update-game-player-cards
  "Give cards to player-id in game."
  [game player-id cards]
  (update-in game
            [:players player-id]
            #(player-cards % cards)))

(defn update-game-player-money
  "Add n money to player-id in game."
  [game player-id n]
  (update-in game
            [:players player-id]
            #(player-money % n)))

(defn deck
  "Generates unshuffled deck or returns deck for game."
  ([] (into [] (for [suit '(\C \D \S \H)
                     value (range 2 15)]
                 (card value suit))))
  ([game] (:deck game)))

(defn money-starting-amount
  "Money for each player depending on n of players."
  [n]
  0)

(defn player-generate
  ""
  ([hand money] {:hand hand, :money money})
  ([] (player-generate nil 0)))

(defn player-active?
  "Return true if player is active in game, false otherwise."
  [game player-id]
  (boolean (some #{player-id} (:active-players game))))

(defn player-set-inactive
  "Adds player-id to active players"
  [game player-id]
  (-> game
      (update-in [:active-players] #(disj % player-id))
      (update-in [:inactive-players] #(conj % player-id))))

(defn player-set-active
  "Adds player-id to active players"
  [game player-id]
  (-> game
      (update-in [:inactive-players] #(disj % player-id))
      (update-in [:active-players] #(conj % player-id))))

(defn hands-active
  "Return active hands."
  [game]
  (map :hand (vals (select-keys (:players game)
                                (:active-players game)))))

(defn turn-order-active
  "Return turn order with only active players."
  [game]
  (filter (set (:active-players game))
          (:turn-order game)))

(defn game-generate
  "Generate a poker game for n players."
  [n]
  {:round 1
   :deck (shuffle (deck))
   :pot 0
   :turn-order (range 1 (inc n))
   :active-players (into #{} (range 1 (inc n)))
   :inactive-players #{}
   :players (zipmap (range 1 (+ 1 n))
                    (repeat n (player-generate nil (money-starting-amount n))))})

;; turn order is small-blind, big blind, ... , dealer

(defn turn-order-advance
  "Advances turn order for game."
  [game]
  (update-in game [:turn-order] #(conj (drop-last %)
                                       (last %))))

(defn game-increase-round-counter
  "Increase game round counter by one."
  [game]
  (update game :round inc))

(defn dealer?
  "Return true if player-id is the dealer, false otherwise."
  [game player-id]
  (let [turn-order-vec (:turn-order game)]
    (= player-id
       (get turn-order-vec (dec (count turn-order-vec))))))

(defn small-blind?
  "Return true if player-id has big blind, false otherwise."
  [game player-id]
  (= player-id
     (get (:turn-order game) 0)))

(defn big-blind?
  "Return true if player-id has big blind, false otherwise."
  [game player-id]
  (= player-id
     (get (:turn-order game) 1)))

(defn player-draw
  "Drawn one card from deck in GAME and give them to PLAYER, return new gamestate.."
  [game player-id]
  (let [deck (deck game)
        drawn-cards (take 1 deck)
        updated-deck (drop 1 deck)]
    (-> game
        (update-game-deck updated-deck)
        (update-game-player-cards player-id drawn-cards))))

(def game-example (game-generate 3))

(defn phase-card-distribution
  "Gives n cards from deck to all players in GAME."
  [game n]
  (nth (iterate (fn [x]
                  (reduce #(player-draw %1 %2)
                          x
                          (turn-order-active game)))
                game)
       n))

(defn value-inter-hand-type
  "Values of hand types."
  [hand]
  (cond (royal-flush? hand) 8000000
        (poker? hand) 7000000
        (full? hand) 6000000
        (flush? hand) 5000000
        (straight? hand) 4000000
        (three-of-a-kind? hand) 3000000
        (two-pair? hand) 2000000
        (pair? hand) 1000000
        :else 0))

(defn value-high-card
  "Numerical value fo a high card hand."
  [hand]
  (let [card-values (sort (map :value hand))
        weights (map #(Math/pow 14 %) (range 0 5))]
    (int (reduce + (map * card-values weights)))))

(defn value-pair
  "Returns numerical value of pair in HAND."
  [hand]
  (let [card-values (sort (distinct (map :value hand)))
        weights (map #(Math/pow 14 %) (range 0 4))]
    (int (+ (reduce + (map * card-values weights))
            (value-inter-hand-type hand)))))

(defn value-two-pair
  "Returns numerical value of two-pair in HAND."
  [hand]
  (let [card-values (sort (distinct (map :value hand)))
        weights (map #(Math/pow 14 %) (range 0 3))]
    (int (+ (reduce + (map * card-values weights))
            (value-inter-hand-type hand)))))

(defn value-three-of-a-kind
  "Returns numerical value of three-of-a-kind in HAND"
  [hand]
  (let [card-values (sort (map :value hand))]
    (int (+ (value-inter-hand-type hand)
            (nth card-values 2)))))

(defn value-full
  "Returns numerical value of full in HAND"
  [hand]
  (int (+ (value-inter-hand-type hand)
          (nth (sort (map :value hand)) 2))))

(defn value-poker
  "Returns numerical value of poker in HAND"
  [hand]
  (int (+ (value-inter-hand-type hand)
          (nth (sort (map :value hand)) 2))))

(defn value-flush
  "Returns numerical value of flush in HAND"
  [hand]
  (int (+ (value-inter-hand-type hand)
          (value-high-card hand))))

(defn value-royal-flush
  "Returns numerical value of poker in HAND"
  [hand]
  (int (+ (value-inter-hand-type hand)
          (value-high-card hand))))

(defn value-straight
  "Numerical value of a straight hand."
  [hand]
  (int (+ (value-inter-hand-type hand)
          (value-high-card hand))))

(defn hand-value
  "Returns value of HAND."
  [hand]
  (cond (royal-flush? hand) (value-royal-flush hand)
        (poker? hand) (value-poker hand)
        (flush? hand) (value-flush hand)
        (full? hand) (value-full hand)
        (straight? hand) (value-straight hand)
        (three-of-a-kind? hand) (value-three-of-a-kind hand)
        (two-pair? hand) (value-two-pair hand)
        (pair? hand) (value-pair hand)
        (high-card? hand) (value-high-card hand)))

(defn hand-name
  "Returns name of HAND as string."
  [hand]
  (cond (royal-flush? hand) "Royal flush"
        (poker? hand) "Poker"
        (flush? hand) "Flush"
        (full? hand) "Full"
        (straight? hand) "Straight"
        (three-of-a-kind? hand) "Three of a kind"
        (two-pair? hand) "Two pair"
        (pair? hand) "Pair"
        (high-card? hand) "High card"))

(defn best-possible-hand
  "Return best hand possible out of list of cards."
  [cards]
  (let [hands (combo/combinations cards 5)
        vals (map hand-value hands)
        m (zipmap vals hands)]
    (apply max-key key m)))

(defn winning-hand
  "Return winning hand."
  [h1 h2 & hs]
  (let [hands (concat (list h1 h2) hs)
        values (map hand-value hands)]
    (get (zipmap values hands) (apply max values))))

(defn player-with-hand
  "Returns player-id for player with hand."
  [players hand]
  (if (empty? players) nil
      (let [p (first players)
            k (first (keys p))
            v (first (vals p))]
        (if (= hand (:hand v))
          k
          (player-with-hand (rest players) hand)))))
