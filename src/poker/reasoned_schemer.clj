(ns poker.reasoned-schemer
  (:require [clojure.core.logic :refer :all]))



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
