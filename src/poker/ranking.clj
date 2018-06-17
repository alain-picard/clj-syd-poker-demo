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

   ;; Will be read as: (beats winner loser)
   [beats :straight-flush :four-of-a-kind]
   [beats :four-of-a-kind :full-house]
   [beats :full-house :flush]           ; Hope I didn't make a mistake
   [beats :flush :straight]             ; in copying the rules.  :-)
   [beats :straight :three-of-a-kind]
   [beats :three-of-a-kind :two-pairs]
   [beats :two-pairs :pair]
   [beats :pair :high-card]))

(db/with-db  poker-rules
  (run* [who]                           ; A pair beats only a high hand
    (beats :pair who)))

(db/with-db  poker-rules
  (run* [who]                           ; a :high-hand beats nobody
    (beats :high-card who)))

(db/with-db  poker-rules                ; nobody beats a straight flush
  (run* [who]
    (beats who :straight-flush)))


;; Comparing any two arbitrary hands,

(defn bettero
  "A relation returning all combinations of WINNING-HANDs capable of beating LOSING-HANDs."
  [losing-hand winning-hand]
  (conde
   [(emptyo losing-hand) u#]
   [(beats winning-hand losing-hand) s#]
   [s#     (fresh [next]
             (beats next losing-hand)
             (bettero next winning-hand))]))

;; What hands can beat :three-of-a-kind ?
(db/with-db poker-rules
  (run* [winner]
     (bettero :three-of-a-kind winner)))


;; What hands can a flush beat ?
(db/with-db poker-rules
  (run* [loser] (bettero loser :flush)))

;; Or just enumerate all possible matchings:
(db/with-db poker-rules
  (run* [loser winner]
    (bettero loser winner)))







;;; Finally -- our poker hand evaluator!

(defn winner
  "Compare the hands of two \"players\", each of which is a pair
   of [player-name poker-hand].  Returns the PLAYER-NAME of the winner
   (or nil, in case of a draw)

   This does NOT handle ranking players by highest card if they have equal types
   of hands---don't base your gambling start-up off of this code!!!"
  [[left-name left-hand] [right-name right-hand]]
  (db/with-db poker-rules
    (first
     (run 1 [q]
       (fresh [winning-hand]
         (conde
          [(== winning-hand right-hand)      ; Read it like this:  If the winning hand is the right-hand
           (bettero left-hand winning-hand)  ; then it must be able to beat the left hand,
           (== q right-name)]                ; therefore our winner is the player associated with right-hand.

          [(== winning-hand left-hand)       ; ibid for the winning hand being the left hand.
           (bettero right-hand winning-hand) ; but now he can beat the _right_ hand.  Careful!
           (== q left-name)]))))))


(winner [:bob :two-pairs] [:alice :flush])
(winner [:bob :three-of-a-kind] [:alice :two-pairs])
(winner [:bob :two-pairs] [:alice :two-pairs])
