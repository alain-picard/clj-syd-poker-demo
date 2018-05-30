(ns poker.reasoned-schemer
  (:require [clojure.core.logic :refer :all]))


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

;; Explain order in which q and x get their values.
(run* (q)
  (fresh [x]
    (== `(:d :a ~x :c) q)
    (conso x `(:a ~x :c) q)))




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

(run* [a a c d e] ; two distinct pairs
  (permuteo [a a c d e] [:a :e :a :d :c])

  ;; (twinso [a b])
  ;; (twinso [d e])
  (distincto [a c d e])
  )

(run 1 [q]
  (fresh [a b c d]
    (== q [[a b] [a d]])
    (permuteo [[a b] [a d]] [[:a :b] [:a :d]])))

(run* [pair]
  (fresh [r a b c d e]
;    (twinso pair)
    (permuteo [a b c] [:x :a :d])
 ;   (appendo pair r )
    ))

(let [h [:b :b :a :c :d]]
 (run* [q]
   (fresh [pair r]
     (twinso pair)
     (conso pair r h)
     (== [pair r] q))))

(run* [r]
  ;(firsto `[a c o r n] 'a)
  (fresh [x]
   (conde
    [(== u# x) (== r :foo)]
    [(== s# x) (== r :bar)]))
  )


(run* [r]
  (fresh [x]
    (conde
     [(firsto `(grape raisin toast) x) (== r :ok)]
     [(firsto `(grape raisin toast) x) (== r :not-ok)])))

;; This is example TRS-2.11
;; Note that we have to use `lcons' rather than scheme's `cons'.
;; (and `firsto' instead of `caro')
;;
(run* [r]
  (fresh [x y]
    (firsto `(grape raisin toast) x)
    (firsto `((a) (b) (c)) y)
    (== (lcons x y) r))) ; ==> ((poker.reasoned-schemer/grape poker.reasoned-schemer/a))

;; TRS-2.24
(run* [r]
  (fresh [x y z]
    (== `(e a d ~x) r)
    (conso y `(a ~z c) r))) ; ==> ((e a d c))

(run* [x]
  (fresh [y]
    ;; (firsto `(~y b c) x)
    (conso 'a (list y 'c) x)
    (== y 'b)
    ))



(run* [q]
  (fresh [x y]
    (permuteo '(a b c d e) q)
    (membero x '(d b))
    (membero y '(d b))
    (conso 'a `(c e ~x ~y) q)))
