(ns poker.core
  (:gen-class))

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

(defn deck
  "Returns an initial unshuffled deck to play with NUMBER-PLAYERS."
  [number-players]
  (for [suit '(\C \Q \F \P)
        value (range (card-lowest-value number-players) 15)]
    (card value suit)))
  




  
  
