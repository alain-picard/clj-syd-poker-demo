(ns poker.reasoned-schemer
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]))

;;; We need to understand the following:
;;;
;;; -  run*
;;; -  == (unify)
;;; -  fresh
;;; -  goals
;;; -  conde


;;; run* is how we "ask questions"
;;;
;;; Unification: == is the basic operator for saying:
;;; attempt to make these two logic variables evaluate to the same value.

(run* [q]
  ;; This is read as: What values can be assigned to q so that
  ;; it will have the value of :foo   (only :foo, obviously)
  (== q :foo))

(run* [q]
  ;; Order doesn't matter --- this is the same question.
  (== :foo q))

(run* [q]
  ;; What values can be assigned to q so that
  ;; it will SIMULTANEOUSLY have the value of :foo and :bar?
  ;; (no such value exists)
  (== q :foo)                          ; All the goals must succeed SIMULTANEOUSLY.
  (== q :bar))



;;; Unification part II:
;;;
;;; You can ask multiple questions, about multiple statements (goals) simultaneously.
;;;

(run* [x]
  (== x "Logic programming rocks!"))  ; But William Byrd prefers to call it "relational programming"...

(run* [p q]
  (== q :foo)
  (== :bar p))

(run* [p q]
  (== q :foo))

(run* [x y]
  (== x y))















;;; The special goal "CONDE" allows us to try multiple alternatives,
;;; thus we can get more than one valid answer to a "question"

(run* [q]
  (conde
   [(== q :bar)]
   [(== q :foo)]))

;;; Special goals, s# and u# (succeed, fail)

(run* [q]
  (conde
   [(== q :bar) s#]
   [(== q :foo) u#]))

(run* [q]
  fail
  (conde
   [(== q :bar) succeed]
   [(== q :foo) fail]))
















;; FRESH: a way to introduce temporary logic variables

(run* [x]    ; Unconstrained!
  (fresh [y]
    (== x y)))



;; "Relational" programming; about describing "relations" between logic variables
;;
;; Questions can be asked about any components.
;; There is not a "place for the output"; any
;; logic variable in any goal can be solved for.

(run* [q]
  (conso :foo '(:bar :baz) q))

(run* [q]
  (conso :foo (list q :baz) '(:foo :bar :baz)))

(run* [q]
  (conso q '(:bar :baz) '(:foo :bar :baz)))











;;;
;;;  Goals:
;;;  ------
;;;  We can write our own goals functions.

(defn twinso [pair] ; What does this do?
  (fresh [x y]
    (== [x y] pair)
    (== x y)))

(defn threeo [triplet]
  (fresh [a b c]
    (== [a b c] triplet)
    (== a b)
    (== a c)))

(run* [a b c d e] ; two distinct pairs
  (permuteo [a b c d e] [:king :three :king :jack :three])
  (twinso [a b])
  (twinso [c d])
  (distincto [a c]))

(run* [a b c d e] ; full house
  (permuteo [a b c d e] [:king :three :king :three :three])
  (threeo [a b c])
  (twinso [d e])) ; Q: Why don't we need the distincto in here?

;; An alternate way of doing it --- does this look clearer??
(run* [a a a b b] ; full house
  (permuteo [a a a b b] [:king :three :king :three :three]))

;; Armed with this, we can now play poker.



;;; Other experimental forms below

(defn some-distincto
  "A relation in which some elements are distinct
   i.e. not all elements are the same."
  [l]
  (fresh [h t]
    (conso h t l)
    (conda                              ; Explain conda
     [(everyg #(== h %) t) u#]
     [s# s#])))

(run* [q]                               ; Clearly broken... FIXME
  (some-distincto [1 1 1 q]))           ; returns (), should return (_.0) ; _.0 != 1

(run* [a b c]
  (== [a b c] [3 b 3])
  (some-distincto [a b c]))

(defmacro fail-if [g]
  `(conda
    [~g fail]
    [succeed]))

(run* [q]
  (== q 7)
  (conda
   [(membero q [2 2 4]) (== q 1) fail]
   [s#]))
