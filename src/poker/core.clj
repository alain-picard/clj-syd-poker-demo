(ns poker.core
  (:refer-clojure :exclude [==])
  (:require
   [clojure.math.combinatorics :as comb :refer [combinations]]
   [clojure.core.logic :refer :all]))


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
  ;; FIXME! This is bogus because it doesn't
  ;; understand that an ace can be either the lowest,
  ;; or highest card, depending.
  (sort card-comp hand))

(defn card-present? [hand card]
  (not= -1 (.indexOf hand card)))

(sort-hand [[:eight :clubs] [:four :clubs] [:ace :hearts] [:jack :diamonds] [:eight :spades]])
;; => ([:ace :hearts] [:jack :diamonds] [:eight :clubs] [:eight :spades] [:four :clubs])


;;;; Some example hands

(def nuthin-hand
  "A hand that I typically get."
  [[:eight :clubs] [:four :clubs] [:ten :hearts] [:jack :diamonds] [:two :spades]])

(def two-of-a-kind-hand
  "A pair of eights."
  [[:eight :clubs] [:four :clubs] [:ten :hearts] [:jack :diamonds] [:eight :spades]])

(def two-pairs-hand
  "A hand with a pair of eights and a pair of tens."
  [[:eight :clubs] [:four :clubs] [:ten :hearts] [:ten :diamonds] [:eight :spades]])

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

(def straight-flush-hand
  "A straight flush, five high"
  [[:two :clubs] [:four :clubs] [:five :clubs] [:ace :clubs] [:three :clubs]])

(def all-test-hands
  '[nuthin-hand two-of-a-kind-hand two-pairs-hand three-of-a-kind-hand straight-hand
   four-of-a-kind-hand full-house-hand high-full-house-hand flush-hand high-flush-hand
   straight-flush-hand])


;;;; Utilities

(defmacro run-1
  "Like the minikanren run-1 form.
  Returns the value of the succeeding clause, or nil."
  [& clauses]
  ;; There is no run-1 in core.logic, and having to
  ;; intersperse (first ...) everywhere is annoying.
  `(first (run 1 ~@clauses)))

;; Turns out I don't need these, but they might
;; be nice to explain.

(defn twinso [s]
  (fresh [x y]
    (conso x [y] s)
    (== x y)))

(defn threeo [s]
  (fresh [a b c]
    (conso a [b c] s)
    (== a b)
    (== a c)))


;;;; Hand classifications --- First attempt

(defn two-of-a-kind?
  [hand]
  (let [ranks (map card-rank hand)]
    (run-1 [a a c d e]
      (permuteo [a a c d e] ranks)
      (distincto [a c d e]))))

;; Get rid of the (map card-rank hand),
;; and try to generalize to a pattern which might
;; work on any hand
;;
(defn two-of-a-kind?
  [hand]
  (run-1 [q]
    (fresh [a b c d e  ; ranks
            r s t u v] ; suites
      (== q [[a r] [b s] [c t] [d u] [e v]]) ; Answer looks like the full hand.
      (permuteo [[a r] [a s] [c t] [d u] [e v]] hand)
      ;; And, redundant, but binding `b' allows us to get
      ;; what looks like a hand, instead of just a fresh val.
      (== a b)
      (distincto [a c d e]))))

;; Again, a sort of "bespoke", crafted solution.
(defn flush?
  "Returns the value of highest ranked card suit if HAND is a flush, otherwise nil."
  [hand]
  (let [hand (sort-hand hand)
        suites (map card-suite hand)] ; If we match, the first card will be the highest in the suit.
    (if (run-1 [suite]
          (== [suite suite suite suite suite] suites))
      (card-rank (first hand)))))

(defn high-card?
  ;; FIXME.
  ;; This is actually hard!  how to distinguish from a flush or straight?
  ;; One way is to cheat and never call this, and have the "fallthrough" case
  ;; be the obvious, high-card? hand.
  [hand]
  (run-1 [rank]
    (fresh [a b c d e]

      (distincto [a b c d e]))))


;;;; A poker specific helper macro

(defmacro define-poker-pred
  "Define a new type of poke predicate named NAME.
   CLAUSES are any numberof forms suitable to be embedded
   in the run* macro.

   Macro is unhygienic, and capture the following symbols:
   (a b c d e) and (r s t u v), where the a,b,... are the ranks
   of the cards, and the r,s... are the suits of those cards.

   The inference is run, and returns either nil, if no match is found,
   or what looks like the matched hand, i.e.
   [[a r] [b s] ...]

   Note that you should ensure that all conditions necessary are met
   so that you do not return unbound (i.e. fresh) symbols, but rather
   a valid permutation of the input hand.

   Functions defined with this macro can then be used thusly:
   (define-poker-pred has-pair? (== a b))
   (has-pair [[:b 1] [:c 2] [:b 3] [:d 1] [:e 4]))
   ==> [[:b 3] [:b 1] [:c 2] [:d 1] [:e 4]]"
  [name & clauses]
  `(defn ~name
     [hand#]
     (run-1 [q#]
       (fresh [~'a ~'b ~'c ~'d ~'e ~' r ~'s ~'t ~'u ~'v]
         ;; Try every permutation of the hand into the available lvars
         (permuteo [[~'a ~'r] [~'b ~'s] [~'c ~'t] [~'d ~'u] [~'e ~'v]] hand#) ; Answer looks like the full hand.
         (== q# [[~'a ~'r] [~'b ~'s] [~'c ~'t] [~'d ~'u] [~'e ~'v]])
         ~@clauses))))



;;;; Armed with define-poker-pred, we can now rewrite our predicates a bit more clearly:

(define-poker-pred two-of-a-kind?
  (== a b)
  (distincto [a c d e]))

(two-of-a-kind? two-of-a-kind-hand)
(two-of-a-kind? two-pairs-hand)

(define-poker-pred two-pairs?
  (== a b)
  (== c d)
  (distincto [a c]))

(define-poker-pred three-of-a-kind?
  [hand]
  (== a b) (== a c) ; and (== b c) is redundant
  (distincto [a d e])) ; Can't have [d e] be the same --- that's a full house!

(define-poker-pred full-house?
  ;; Note, there is no need to check for (distinct [a d])
  ;; as there cannot be 5 cars of the same rank in a hand (under these rules).
  (== a b) (== a c) ; and (== b c) is redundant
  (== d e))

(define-poker-pred flush?
  ;; A hand is flush if the 2nd, third card etc all
  ;; have the same suite as the first one.
  (everyg #(== r %) [s t u v]))

(define-poker-pred four-of-a-kind?
  ;; n.b. since four-of-a-kind is higher than a straight,
  ;; we don't need to check to see if the suites differ.  (I think)
  (== a b)
  (== a c)
  (== a d))

;; The straight is tricky, so I simply enumerate every possibility:

(def all-possible-straights
  (let [all (concat [:ace] ranks)]
    (for [n (range 10)]
      (into []
            (->> all
                 (drop n)
                 (take 5))))))

;; =>> ([:ace :two :three :four :five] [:two :three :four :five :six]
;;      [:three :four :five :six :seven] [:four :five :six :seven :eight] ... etc
;;      [:ten :jack :queen :king :ace])
;; Note that the :ace can be either highest or lowest!

;; And with that, it becomes trivial:

(define-poker-pred straight?
  (membero [a b c d e] all-possible-straights))

(define-poker-pred straight-flush?
  (membero [a b c d e] all-possible-straights)
  (everyg #(== r %) [s t u v]))

(defn some-distinct? [l]
  (cond
    (empty? l)  nil
    :else (let [[h & r] l]
            (not (empty? (filter #(not= h %) r))))))

(defn some-distincto
  "A relation in which some elements are distinct
   i.e. not all elements are the same."
  [l]
  (fresh [h t]
    (conso h t l)
    (conda                              ; Explain conda
     [(everyg #(== h %) t) u#]
     [s# s#])))

(define-poker-pred high-card?
  (distincto [a b c d e])
  (some-distincto [r s t u v]))

(high-card? flush-hand)
(high-card? nuthin-hand)




;;;; Hand validation

(define-poker-pred valid?
  ;; Every rank and suite is valid
  (everyg #(membero % ranks) [a b c d e])
  (everyg #(membero % suites) [r s t u v])
  ;; And we have no duplicate cards
  (distincto [[a r] [b s] [c t] [d u] [e v]]))



;;;; Tests
(defn- check [pred hand]
  (assert (pred hand)))

(defn- check-not [pred hand]
  (assert (not (pred hand))))

(def all-preds
  [[:straight-flush   straight-flush?]
   [:four-of-a-kind   four-of-a-kind?]
   [:full-house       full-house?]
   [:flush            flush?]
   [:straight         straight?]
   [:three-of-a-kind  three-of-a-kind?]
   [:two-of-a-kind    two-of-a-kind?]
   [:two-pairs-hand   two-pairs?]
   [:high-card        high-card?]])

(defn categorize [hand]
  (first (filter identity
                 (map (fn [[name p]] (when (p hand) [name hand]))
                      all-preds))))

(defn categorize [hand]
  (->> all-preds
       (map (fn [[name p]] (when (p hand) [name hand])))
       (filter identity)
       (first)))


(def results
  (for [h all-test-hands]
    [h (categorize (eval h))]))

#_
(let [s (rand-nth all-test-hands)
      h (eval s)]
  (println s)
  (println h)
 (categorize  h))


(def results (atom {}))

(comment
  (dotimes [i 50]
    (let [[cat h] (categorize (first (draw (shuffle deck) 5)))]
      (assert cat (str "unable to categorize" h))
      (swap! results update cat (fnil inc 0)))
    (println i)))



(time
 (do
   (check valid? nuthin-hand)

   ;; Nothing matchtes the nuthin-hand:
   (doseq [[_ p] all-preds]
     (check-not p nuthin-hand))

   (check two-of-a-kind? two-of-a-kind-hand)
   (check-not two-of-a-kind? three-of-a-kind?)

   (check three-of-a-kind? three-of-a-kind-hand)
   (check-not three-of-a-kind? full-house-hand)

   (check full-house? full-house-hand)
   (check straight? straight-hand)
   (check flush? flush-hand)))








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

(defne my-membero
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
    (and*
     [(== high (card-rank (first hand)))
      (== [suite suite suite suite suite] suites)])))

(let [hand flush-hand #_ test-hand]
  (run* [s hi]
    (flusho hand s hi)))


(time (straight? straight-hand))