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
   [hand :flush]                        ; These are all the kinds of hands which exist
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


;;; Examples of looking up the rules


(db/with-db  poker-rules
  (run* [who]                           ; A pair beats only a high hand
    (beats :pair who)))

(db/with-db  poker-rules
  (run* [who]                           ; a :high-hand beats nobody
    (beats :high-card who)))

(db/with-db  poker-rules                ; nobody beats a straight flush
  (run* [who]
    (beats who :straight-flush)))








;; Now generalize this idea to any two arbitrary hands,
;; by creating a new kind of "goal"

(defn bettero
  "A relation returning all combinations of WINNING-HANDs capable of beating LOSING-HANDs."
  [winning-hand losing-hand]
  (conde
   [(emptyo winning-hand) u#]
   [(beats winning-hand losing-hand) s#]
   [s#     (fresh [next]
             (beats next losing-hand)
             (bettero winning-hand next))]))

;; What hands can beat :three-of-a-kind ?
(db/with-db poker-rules
  (run* [winner]
     (bettero winner :three-of-a-kind)))


;; What hands can a flush beat ?
(db/with-db poker-rules
  (run* [loser] (bettero :flush loser)))

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
           (bettero winning-hand left-hand)  ; then it must be able to beat the left hand,
           (== q right-name)]                ; therefore our winner is the player associated with right-hand.

          [(== winning-hand left-hand)       ; ibid for the winning hand being the left hand.
           (bettero winning-hand right-hand) ; but now he can beat the _right_ hand.  Careful!
           (== q left-name)]))))))




(winner [:bob :two-pairs] [:alice :flush])
(winner [:bob :three-of-a-kind] [:alice :two-pairs])
(winner [:bob :two-pairs] [:alice :two-pairs])


;; An alternate implementation;
;; Not sure if it's easier to read or not.

#_
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
       (fresh [winning-hand losing-hand]
         (bettero winning-hand losing-hand)
         (conde
          [(== winning-hand right-hand)      ; Read it like this:  If the winning hand is the right-hand
           (== losing-hand left-hand)
           (== q right-name)]                ; therefore our winner is the player associated with right-hand.

          [(== winning-hand left-hand)       ; ibid for the winning hand being the left hand.
           (== losing-hand right-hand)
           (== q left-name)]))))))
