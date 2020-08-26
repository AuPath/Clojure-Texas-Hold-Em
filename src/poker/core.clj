(ns poker.core
  (:gen-class)
  (:require clojure.set))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;;; Numero massimo di giocatori è 6
;;; Come quando fuori piove, cuori quadri fiori picche
;;; A,K,Q,J,10,9,8,7,6,5
;;; 14,13,12,11,10,9,8,7,6,5
;;; C,Q,F,P
;;; R,B

(defn card-lowest-value
  "Returns value of the card that has the lowest value."
  [number-players]
  (- 11 number-players))

(defn card
  "Returns a card with VALUE, SUIT."
  [value suit] {:value value, :suit suit})

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

(defn deck-generate
  "Returns an initial unshuffled deck to play with NUMBER-PLAYERS."
  [number-players]
  (into [] (for [suit '(\C \Q \F \P)
                 value (range (card-lowest-value number-players) 15)]
             (card value suit))))

;; {:deck '(list of cards)
;;  :discarded-cards '(list of cards)
;;  :pot number
;;  :players '({:hand '(list of cards)
;;              :money number})}

(defn update-game-pot
  "Update pot by AMOUNT"
  [game amount]
  (update game :pot #(+ % amount)))

(defn update-game-deck
  "Return new gamestate with DECK."
  [game deck]
  (assoc game :deck deck))

(defn update-game-player
  "Returns new game state where previous GAME state has been updated with PLAYER at PLAYER-ID."
  [game player-id player]
  (assoc-in game
             [:players player-id]
             player))

(defn player-cards
  "Gives CARDS to PLAYER."
  [player cards]
  (assoc player :hand cards))

(defn player
  "Returns player with PLAYER-ID from GAME."
  [game player-id]
  (get (:players game) player-id))

(defn player-money
  "Adds AMOUNT to PLAYER money."
  [player amount]
  (update player :money #(+ % amount)))

(defn deck
  "Returns GAME deck."
  [game]
  (:deck game))

(def game-example {:deck (shuffle (deck-generate 4))
                         :pot 0
                   :players {1 {:hand nil, :money 0}
                             2 {:hand nil, :money 100}}})

(defn money-starting-amount
  "Money for each player depending on n of players."
  [n]
  0)

(defn player-generate
  ""
  ([hand money] {:hand hand, :money money})
  ([] (player-generate nil 0)))

(defn game-generate
  "Generate a poker game for n players."
  [n]
  {:deck (shuffle (deck-generate n))
   :pot 0
   :players (zipmap (range 1 (+ 1 n))
                    (repeat n (player-generate nil (money-starting-amount n))))
   })

(defn phase-blind
  "Removes BLIND amount of money from all players."
  [game blind]
  (reduce #(update-game-player %1
                               %2
                               (player-money (player %1 %2)
                                             (- blind)))
          game
          (keys (:players game))))

(defn player-draw-n-cards-from-deck
  "Drawn N cards from deck in GAME and give them to PLAYER, return new gamestate."
  [game player-id n]
  (let [deck (deck game)
        drawn-cards (take n deck)
        updated-deck (drop n deck)
        new-gamestate (update-game-deck game updated-deck)]
    (update-game-player new-gamestate
                        player-id
                        (player-cards (player new-gamestate player-id)
                                      drawn-cards))))

(defn phase-card-distribution
  "Gives 5 cards from deck to all player in GAME."
  [game]
  (reduce #(player-draw-n-cards-from-deck %1 %2 5)
          game
          (keys (:players game))))

(defn update-game-bet
  "Bet AMOUNT, take money from player with PLAYER-ID and add it to GAME pot. Returns new gamestate."
  [game player-id amount]
  (-> game
      (update-game-player player-id
                          (player-money (player game player-id)
                                        (- amount)))
      (update-game-pot amount)))

(defn value-inter-hand-type
  ""
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
  ""
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
  ""
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




  
