(ns poker.ranking
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.core.logic.pldb :as db]))

;; This define a fact called "hand"; i.e. enumerate what sort of hands exist.
(db/db-rel hand h)

;; This define a fact called "beats"; i.e. enumerate the order in which hands are sorted.
(db/db-rel beats left right)


;;; This is how we teach core.logic about the rules of poker

(def poker-rules
  (db/db
   [hand :straight-flush]
   [hand :four-of-a-kind]
   [hand :flush]                        ; These are all our hands
   [hand :straight]
   [hand :three-of-a-kind]
   [hand :two-pairs]
   [hand :pair]
   [hand :high-card]

   [beats :straight-flush :four-of-a-kind]
   [beats :four-of-a-kind :full-house]
   [beats :full-house :flush]           ; Hope I didn't make a mistake
   [beats :flush :straight]             ; And this is who beats what.
   [beats :straight :three-of-a-kind] ; in copying the rules.  :-)
   [beats :three-of-a-kind :two-pairs]
   [beats :two-pairs :pair]
   [beats :pair :high-card]))


(db/with-db  poker-rules
  (run* [who]                           ; A pair beats only a high hand
    (beats :pair who)))

(db/with-db  poker-rules
  (run* [who]                           ; a :high-hand beats nobody
    (beats :high-card who)))

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
     (bettero :three-of-a-kind winner)))



;;; Finally -- our poker hand evaluator!

(defn winner
  "Compare the hands of two \"players\", each of which is a pair
   of [player-name poker-hand].  Returns a map indicating the winner and loser
   (or nil, in case of a draw)

   This does NOT handle ranking players by highest card if they have equal types
   of hands---don't base your gambling start-up off of this code!!!"
  [player-left player-right]
  (db/with-db poker-rules
    (run* [q]
      (fresh [losing-hand loser-name winning-hand winner-name loser winner]
        (bettero losing-hand winning-hand) ; Enumerates all pairs of which hand beats which
        (== loser  [loser-name losing-hand])   ; loser, winner are just helper vars for clarity
        (== winner [winner-name winning-hand])
        (conde ; We have 2 cases; either the winning player is player-right, or he's player-left.
         [(all (== winner player-right)
               (== loser  player-left)
               (== q {:winner player-right
                      :loser player-left})) s#]
         [(all (== winner player-left)           ; Winning player is player-left
               (== loser  player-right)
               (== q {:winner player-left
                      :loser player-right})) s#])))))


(winner [:bob :two-pairs] [:alice :flush])
(winner [:bob :three-of-a-kind] [:alice :two-pairs])
(winner [:bob :two-pairs] [:alice :two-pairs])
