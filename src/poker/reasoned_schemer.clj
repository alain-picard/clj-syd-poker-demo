(ns poker.reasoned-schemer
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]))

;;; We need to understand the following:
;;;
;;; -  run*
;;; -  == (unify)
;;; -  fresh
;;; -  goals

(run* [q]
  (== q :foo))

(run* [q]
  (== q :foo)
  (== q :bar))

(run* [q]
  (== q :foo)
  (== q :foo))

(run* [q]
  (conde
   ((== q :bar) s#)
   [(== q :foo) s#]))


;; Questions can be asked about any components.

(run* [q]
  (conso :foo '(:bar :baz) q))

(run* [q]
  (conso :foo `(~q :baz) '(:foo :bar :baz)))

(run* [q]
  (conso q '(:bar :baz) '(:foo :bar :baz)))


;;; The top

(run* [x y]
  (== x y))

(run* [x]
  (fresh [y]
    (== x y)))

;; Explain order in which q and x get their values.
(run* (q)
  (fresh [x]
    (== `(:d :a ~x :c) q)
    (conso x `(:a ~x :c) q)))



;; All the statements inside of the run* must succeed.

(run* [q]
  (nilo '(:a :b :c))
  (== s# q))

(run* [q]
  (nilo nil)
  (== s# q))



(defn twinso [s]
  (fresh [x y]
    (conso x [y] s)
    (== x y)))

(defn threeo [s]
  (fresh [a b c]
    (conso a [b c] s)
    (== a b)
    (== a c)))

(run* [a b c d e] ; two distinct pairs
  (permuteo [a b c d e] [:a :e :a :d :e])
  (twinso [a b])
  (twinso [c d])
  (distincto [a c]))

(run* [a b c d e] ; full house
  (permuteo [a b c d e] [:a :e :a :a :e])
  (threeo [a b c])
  (twinso [d e]))

;; Armed with this, we can now play poker.
