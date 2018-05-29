(ns poker.core
  (:refer-clojure :exclude [==])
  (:require
   [clojure.math.combinatorics :as comb :refer [combinations]]
   [clojure.core.logic :refer [run* == fresh membero distincto] :as l]))


;;;;  Card Abstract data types.

(def ranks
  "The face values in a suite of cards"
  [:two :three :four :five :six :seven :eight :nine :ten :jack :queen :king :ace])

(defn rank-value [rank] (.indexOf ranks rank))

(def suites
  "The suites in a deck of cards"
  [:spades :clubs :hearts :diamonds])

;; Note that the official rules of poker do not indicate a ranking
;; of suites; thus you can have official "ties" in a poker hand.
;; e.g.  [[:king :hearts]   [:king :clubs]  [:four :spades] [:three :spades] [:two :spades]]
;;  ==   [[:king :diamonds] [:king :spades] [:four :diamonds] [:three :diamonds] [:two :diamonds]]
;;
;; Yeah, this surprised me as well.

(defn card
  "Produce a card of rank RANK in suite SUITE"
  [rank suite]
  [rank suite])

(defn card-rank [card] (first card))
(defn card-suite [card] (second card))

(defn card-value [card]
  (rank-value (card-rank card)))

(def deck
  "A regular deck of cards.
  To shuffle a deck, use the built-in clojure.core/shuffle function."
  (for [suite suites
        rank ranks]
    (card rank suite)))

(assert (= 52 (count deck)))

(defn draw
  "Draw N cards from a deck of cards.
   Returns the hand, and the remaining deck."
  [deck n]
  (let [hand (into [] (take n deck))
        rest (drop n deck)]
    [hand rest]))

(defn draw-two-poker-hands []
  (let [deck (shuffle deck)
        [left deck] (draw deck 5)
        [right _] (draw deck 5)]
    [left right]))

(defn card-comp [a b]
  (> (card-value a) (card-value b)))

(defn sort-hand [hand]
  (sort card-comp hand))

(defn card-present? [hand card]
  (not= -1 (.indexOf hand card)))

(sort-hand [[:eight :clubs] [:four :clubs] [:ace :hearts] [:jack :diamonds] [:eight :spades]])
;; => ([:ace :hearts] [:jack :diamonds] [:eight :clubs] [:eight :spades] [:four :clubs])


(def two-of-a-kind-hand
  "A pair of eights."
  [[:eight :clubs] [:four :clubs] [:ten :hearts] [:jack :diamonds] [:eight :spades]])

(def three-of-a-kind-hand
  "A triple of eights."
  [[:eight :clubs] [:four :clubs] [:eight :hearts] [:jack :diamonds] [:eight :spades]])

(def straight-hand
  "A straight, five high."
  [[:two :diamonds] [:four :clubs] [:five :spades] [:ace :clubs] [:three :clubs]])

(def four-of-a-kind-hand
  "A hand with four eights."
  [[:eight :clubs] [:four :clubs] [:eight :hearts] [:eight :diamonds] [:eight :spades]])

(def full-house-hand
  "A full house, eight high."
  [[:eight :clubs] [:four :clubs] [:eight :hearts] [:eight :diamonds] [:four :spades]])

(def high-full-house-hand
  "A full house, King high."
  [[:eight :clubs] [:king :clubs] [:eight :hearts] [:king :diamonds] [:king :spades]])

(def flush-hand
  "A flush hand, jack high."
  [[:eight :clubs] [:four :clubs] [:jack :clubs] [:five :clubs] [:three :clubs]])

(def high-flush-hand
  "A flush hand, ace high."
  [[:eight :clubs] [:four :clubs] [:jack :clubs] [:ace :clubs] [:three :clubs]])


;;;; Utilities

(defmacro run-1
  "Like the minikanren run-1 form.
  Returns the value of the succeeding clause, or nil."
  [& clauses]
  ;; There is no run-1 in core.logic, and having to
  ;; intersperse (first ...) everywhere is annoying.
  `(first (l/run 1 ~@clauses)))


;;;; Hand classification

(defn two-of-a-kind?
  "Return the value of the rank of a three of a kind hand, or nil."
  [hand]
  (run-1 [rank]
    (fresh [suit-1 suit-2 c suit-3 d suit-4 e suit-5]
      (l/membero [rank suit-1] hand)
      (l/membero [rank suit-2] hand)
      (l/membero [c suit-3] hand)
      (l/membero [d suit-4] hand)
      (l/membero [e suit-5] hand)
      (l/distincto [rank c d e])
      (l/distincto [suit-1 suit-2]))))


(defn three-of-a-kind?
  "Return the value of the rank of a three of a kind hand, or nil."
  [hand]
  (run-1 [rank]
    (fresh [a b c]
     (l/membero [rank a] hand)
     (l/membero [rank b] hand)
     (l/membero [rank c] hand)
     (l/distincto [a b c]))))

(defn four-of-a-kind?
  "Return the value of the rank of a four of a kind hand, or nil."
  [hand]
  (run-1 [rank]
    (fresh [a b c d]                  ; a,b,c,d are suits.
      (membero [rank a] hand)
      (membero [rank b] hand)
      (membero [rank c] hand)
      (membero [rank d] hand)
      (distincto [a b c d]))))

(defn full-house?
  "Returns a vector of [triple pair] if HAND is a full house, otherwise nil."
  [hand]
  (run-1 [hi lo]
    (fresh [a b c d e]
      (membero [hi a] hand)
      (membero [hi b] hand)
      (membero [hi c] hand)

      (membero [lo d] hand)
      (membero [lo e] hand)

      (distincto [hi lo])
      (distincto [a b c])
      (distincto [d e]))))

(defn flush?
  "Returns the value of highest ranked card suit if HAND is a flush, otherwise nil."
  [hand]
  (let [hand (sort-hand hand)
        suites (map card-suite hand)] ; If we match, the first card will be the highest in the suit.
    (if (run-1 [suite]
          (== [suite suite suite suite suite] suites))
      (card-rank (first hand)))))

(def all-possible-straights
  (let [all (concat [:ace] ranks)]
    (for [n (range 10)]
      (into []
            (->> all
                 (drop n)
                 (take 5))))))
(defn straight?
  "Returns the value of highest ranked card suit if HAND is a flush, otherwise nil.
  :ace is returned if it is in the \"highest\" position, i.e. above :king, but not
  if it is in the \"lowest\" position, i.e. below :two "
  [hand]
  (let [ranks (map card-rank hand)] ; No need to recompute this every time.
    (run-1 [e] ; The last card in the straight
      (fresh [a b c d] ; The 4 lowest cards.
        ;; Remember, whatever gets bund to E will have the highest rank,
        ;; because it will be rightmost.  This takes care of the Ace being
        ;; lowest/highest.
        (l/permuteo [a b c d e] ranks)
        (l/membero [a b c d e] all-possible-straights)))))



(comment                                ; Some simple tests

  (two-of-a-kind? two-of-a-kind-hand)
  (two-of-a-kind? three-of-a-kind-hand)

  (two-of-a-kind test-hand)
  (two-of-a-kind (first (draw (shuffle deck) 5)))

  (two-of-a-kind
   [[:ten :spades] [:jack :spades] [:eight :hearts] [:three :hearts] [:ace :hearts]])


  (nil? (full-house four-of-a-kind-hand))

  (= [:ten :jack]
     (full-house [[:ten :spades] [:jack :spades] [:jack :hearts] [:ten :clubs] [:ten :diamonds]]))

  ;; Order doesn't matter
  (= [:ten :jack]
     (full-house
      (shuffle
       [[:ten :spades] [:jack :spades] [:jack :hearts] [:ten :clubs] [:ten :diamonds]])))

  (two-of-a-kind two-of-a-kind-hand)
  (three-of-a-kind three-of-a-kind-hand)
  (four-of-a-kind four-of-a-kind-hand)
  (full-house full-house-hand)

  (= :ace (flush flush-hand)))


(defn categorize-hand [hand]
  (condp #(%1 %2) hand
    two-of-a-kind?     :>> #(vector :pair %)
    full-house?        :>> #(vector :full-house %)
    four-of-a-kind?    :>> #(vector :four-of-a-kind %)
    three-of-a-kind?   :>> #(vector :three-of-a-kind %)
    ;; FIXME: Needs the "busted" hand.
    ))

(def sorted-rules
  [four-of-a-kind? full-house? three-of-a-kind? two-of-a-kind?])

(let [hand three-of-a-kind-hand]
 (map #(% hand) sorted-rules))

(categorize-hand full-house-hand)
(categorize-hand four-of-a-kind-hand)
(categorize-hand three-of-a-kind-hand)

(comment
  (map sort-hand
       (draw-two-poker-hands))
  (([:jack :diamonds] [:nine :spades] [:nine :diamonds] [:six :clubs] [:five :diamonds])
   ([:jack :clubs] [:ten :clubs] [:nine :clubs] [:three :hearts] [:three :diamonds])))


;;;;  Poker rules

(def winning-order
  [:high-card                           ; FIXME: tbd
   :pair
   :two-pairs                           ; FIXME: tbd
   :three-of-a-kind
   :straight
   :flush
   :full-house
   :four-of-a-kind
   :straight-flush])                    ; FIXME: tbd


;; How do we compare hands?
(defn wins?
  "Returns the winning hand (or both, if a tie)"
  [left right] ; These are hand classifications.
  (let [l (.indexOf winning-order left)
        r (.indexOf winning-order right)]
    (cond
      (< l r) right
      (> l r) left
      :else   [left right])))

(defn wins? [left right]
  (first
   (drop-while nil?
               (mapcat
                (fn [w]
                  (conj
                   []
                   (when (= w right)
                     [:winner :right right])
                   (when (= w left)
                     [:winner :left left])))
                (reverse winning-order)))))

(defn wins? [left right]
  ;; None of this takes care of ties within hands;
  ;; e.g. two hands with a pair, highest pair wins.
  ;; FIXME.
  (when (not= left right) ; Ties get reported as nil.
    (->> (mapcat (fn [w] (conj []
                               (when (= w right)
                                 [:right right])
                               (when (= w left)
                                 [:left left])))
                 (reverse winning-order))
         ;; The first one to have been able to place
         ;; his entry on the list is the winner!
         (drop-while nil?)
         first)))

(let [[position hand] (wins? :straight :flush)]
  position)


;;;; Other experiments below

(l/defne my-membero
  "A relation where l is a collection, such that l contains x."
  [x l]
  ([_ [x . tail]])
  ([_ [head . tail]]
    (my-membero x tail)))


(defn flusho
  "Returns the value of highest ranked card suit if HAND is a flush, otherwise nil."
  ;; This is not a real relation, because it cannot infer hand if passed suite, high.
  [hand suite high]
  (let [hand (sort-hand hand)
        suites (map card-suite hand)] ; If we match, the first card will be the highest in the suit.
    (l/and*
     [(== high (card-rank (first hand)))
      (== [suite suite suite suite suite] suites)])))

(let [hand flush-hand #_ test-hand]
  (run* [s hi]
    (flusho hand s hi)))


(time (straight? straight-hand))
