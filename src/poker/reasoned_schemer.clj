(ns poker.reasoned-schemer
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]))

;;; We need to understand the following:
;;;
;;; -  run*
;;; -  == (unify)
;;; -  fresh
;;; -  goals


;;; run* is how we "ask questions"
;;;
;;; Unification: == is the basic operator for saying:
;;; make these two logic variables evaluate to the same value.

(run* [q]
  ;; This is read as: What values can be assigned to q so that
  ;; it will have the value of :foo   (only :foo, obviously)
  (== q :foo))

(run* [q]
  ;; What values can be assigned to q so that
  ;; it will SIMULTANEOUSLY have the value of :foo and :bar?
  ;; (no such value exists)
  (== q :foo)
  (== q :bar))

(run* [q]
  (== q :foo)
  (== q :foo))




;; Questions can be asked about any components.
;; There is not a "place for the output"; any
;; logic variable in any goal can be solved for.

(run* [q]
  (conso :foo '(:bar :baz) q))

(run* [q]
  (conso :foo `(~q :baz) '(:foo :bar :baz)))

(run* [q]
  (conso q '(:bar :baz) '(:foo :bar :baz)))










;;; The special goal "conde" allows us to try multiple alternatives:

(run* [q]
  (conde
   ((== q :bar) s#)
   [(== q :foo) s#]))
















;;; Unification part II:
;;; Unification: == is the basic operator for saying:
;;; make these two logic variables evaluate to the same value.

(run* [x]
  (== x "Logic programming rocks!"))

(run* [x y]  ; Unconstrained!
  (== x y))

(run* [x]    ; Unconstrained!
  (fresh [y]
    (== x y)))

;; Explain order in which q and x get their values.

(run* (q)
  (fresh [x]
    (== `(:d :a ~x :c) q)
    (conso x `(:a ~x :c) q)))







;; All the statements inside of the run* must succeed.

;; s# means "successful".
;; u# means "unsuccessful"

(run* [q]
  (nilo '(:a :b :c))
  (== s# q))

(run* [q]
  (nilo nil)
  (== s# q))









;;;  We can write our own goals functions.

(defn twinso [pair] ; What does this do?
  (fresh [x y]
    (conso x [y] pair)
    (== x y)))

(defn threeo [triplet]
  (fresh [a b c]
    (conso a [b c] triplet)
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
  (twinso [d e]))

;; An alternate way of doing it --- does this look clearer??
(run* [a a a b b] ; full house
  (permuteo [a a a b b] [:king :three :king :three :three]))

;; Armed with this, we can now play poker.
