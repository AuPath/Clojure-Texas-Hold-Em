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
  [value suit]
  {:value value
   :suit suit
   :color (if (or (= \C suit)
                  (= \Q suit))
            \R
            \N)})

(defn group-of-n?
  [n hand]
  (boolean
   (some #(< n %)
         (vals (frequencies (map :value hand))))))

(defn pair?
  [hand]
  (group-of-n? 2 hand))

(defn tris?
  [hand]
  (group-of-n? 3 hand))

(defn poker?
  [hand]
  (group-of-n? 4 hand))

(defn colore?
  [hand]
  (= 1 (count (group-by :suit hand))))

(defn scala?
  [hand]
  (let [values (sort (map :value hand))]
    (= values
       (range (apply min values) (+ 1 (apply max values))))))

(defn doppia-coppia?
  [hand]
  (= 2
     (count (filter #(= 2 %)
                    (vals (frequencies (map :value hand)))))))

(defn scala-reale?
  [hand]
  (and (scala? hand)
       (colore? hand)))

(defn full?
  [hand]
  (and (tris? hand)
       (pair? hand)))

(defn carta-alta?
  [hand]
  (not
   ((some-fn pair? doppia-coppia? tris?
             poker? scala? scala-reale? full?) hand)))

(defn deck-generate
  "Returns an initial unshuffled deck to play with NUMBER-PLAYERS."
  [number-players]
  (for [suit '(\C \Q \F \P)
        value (range (card-lowest-value number-players) 15)]
    (card value suit)))

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

(defn deck-add-cards
  "Adds CARDS to DECK."
  [deck cards]
  (vec (clojure.set/union (set deck)
                         (set cards))))

(defn deck-remove-cards
  "Removes CARDS to DECK."
  [deck cards]
  (vec (clojure.set/difference (set deck)
                              (set cards))))

;; (defn draw-n-cards-from-deck
;;   ""
;;   [game player-id n]
;;   (let [deck (:deck game)
;;         cards-drawn (take n deck)
;;         new-game (assoc game :deck (clojure.set/difference (set deck)
;;                                                            (set cards-drawn)))]
;;     (update-player-cards new-game player-id cards-drawn)))


(def game-example {:deck (shuffle (deck-generate 4))
                         :pot 0
                   :players {1 {:hand nil, :money 0}
                             2 {:hand nil, :money 100}}})

(defn cards-discarded
  "Returns discarded cards."
  [game]
  (vec
   (clojure.set/difference (set (:deck game))
                           (set (apply merge (map :hand (vals (:players game))))))))

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
        updated-deck (deck-remove-cards deck drawn-cards)
        new-gamestate (update-game-deck game updated-deck)]
    (update-game-player new-gamestate
                        player-id
                        (player-cards (player new-gamestate player-id)
                                      drawn-cards))))

(defn phase-card-distribution
  ""
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

(defn value-carta-alta
  ""
  [hand]
  (map #(biginteger (Math/pow % %2))
                 '(9 10 11 12 13)
                 '(1 2 3 4 5)))

(def esempio-carta-alta (list (card 14 \C)
                              (card 13 \C)
                              (card 12 \F)
                              (card 10 \C)
                              (card 9 \C)))

(def esempio-coppia (list (card 14 \C)
                          (card 14 \C)
                          (card 12 \F)
                          (card 10 \C)
                          (card 9 \C)))

(def esempio-tris (list (card 14 \C)
                        (card 14 \C)
                        (card 14 \F)
                        (card 10 \C)
                        (card 9 \C)))

(def esempio-poker (list (card 14 \C)
                         (card 14 \C)
                         (card 14 \F)
                         (card 14 \C)
                         (card 9 \C)))

(def esempio-full (list (card 14 \C)
                        (card 14 \C)
                        (card 14 \F)
                        (card 10 \C)
                        (card 10 \C)))

(def esempio-scala (list (card 14 \C)
                         (card 13 \C)
                         (card 12 \F)
                         (card 11 \C)
                         (card 10 \C)))

(def esempio-scala-colore (list (card 14 \C)
                          (card 13 \C)
                          (card 12 \C)
                          (card 11 \C)
                          (card 10 \C)))

(def esempio-colore (list (card 14 \C)
                          (card 9 \C)
                          (card 12 \C)
                          (card 11 \C)
                          (card 10 \C)))


  
