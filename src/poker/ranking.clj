(ns poker.ranking
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.core.logic.pldb :as db]))

;; This define a fact called "hand"; i.e. enumerate what sort of hands exist.
(db/db-rel hand h)

;; This define a fact called "beats"; i.e. enumerate the order in which hands are sorted.
(db/db-rel beats left right)

(def poker-rules
  (db/db
   [hand :flush]                        ; These are all our hands
   [hand :straight]
   [hand :three-of-a-kind]
   [hand :two-pairs]
   [hand :pair]
   [hand :high-hand]

   [beats :straight :flush]             ; And this is who beats what.
   [beats :flush :full-house]           ; Hope I didn't make a mistake
   [beats :full-house :three-of-a-kind] ; in copying the rules.  :-)
   [beats :three-of-a-kind :two-pairs]
   [beats :two-pairs :pair]
   [beats :pair :high-hand]))


(db/with-db  poker-rules
  (run* [who]                           ; A pair beats only a high hand
    (beats :pair who)))

(db/with-db  poker-rules
  (run* [who]                           ; a :high-hand beats nobody
    (beats :high-hand who)))

(db/with-db  poker-rules                ; nobody beats a straight
  (run* [who]
    (beats who :straight)))


(defn bettero
  "A relation returning all hands capable of beating HAND."
  [hand who]
  (conde
   [(emptyo hand) s#]
   [(beats who hand) s#]
   [s#     (fresh [next]
             (beats next hand)
             (bettero next who))]))

(db/with-db  poker-rules
  (run* [winner]
     ;; Should return all hands which can beat :three-of-a-kind
     (bettero :two-pairs winner)))


(defn winner [player-left player-right]
  (db/with-db poker-rules
    (run* [q]
      (fresh [losing-hand loser-name
              winning-hand winner-name]
        (bettero losing-hand winning-hand)
        (conde
         ;; We have 2 cases; either the winning player
         ;; is player-right, or he's player-left.

         ;; Winning player is player-right
         [(all (== [loser-name losing-hand]   player-left)
               (== [winner-name winning-hand] player-right)
               (== q {:winner player-right  :loser player-left})) s#]

         ;; Winning player is player-left
         [(all (== [loser-name losing-hand]   player-right)
               (== [winner-name winning-hand] player-left)
               (== q {:winner player-left  :loser player-right})) s#])))))

(winner [:bob :two-pairs] [:alice :flush])
(winner [:bob :three-of-a-kind] [:alice :two-pairs])
(winner [:bob :two-pairs] [:alice :two-pairs])   ; This case is not yet handled.
