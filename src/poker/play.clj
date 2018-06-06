(ns poker.play
  (:require [clojure.core.logic :refer :all]))

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


(run* [q]
  (fresh [a b c d x y]
    ;; [:spades :clubs :clubs  :clubs    :clubs]
    (permuteo [a b c d] [:spades :clubs :hearts :diamonds :clubs])
    (== [x y] [a b])
    (conde
     [(== x y) u#]
     [u#       s#])))

(let [suites
      [:spades :clubs :hearts :diamonds :clubs]
;      [:clubs :clubs :hearts :diamonds :clubs]
;     [:clubs :clubs :hearts :clubs :clubs]
;      [:clubs :clubs :clubs :clubs :clubs]
      ]
  (run 1 [q]
    (fresh [a b r r2 perms]
      (conso a r suites)
      (membero b r)
      (!= a b)
      (== q [a b]))))


;; distincto eventually macroexpands to this:
(fn [l]
  (conde
    ((fresh [] (== () l)))
    ((fresh [h] (== [h] l)))
    ((fresh
       [t h1 h0]
       (== (llist h0 h1 t) l)
       (!= h0 h1)
       (distincto (lcons h0 t))
       (distincto (lcons h1 t))))))

(defn mydistincto [l]
  (conde
   ((fresh [] (== () l)))
   ((fresh [h]
      (== [h] l)
      (trace-lvars "ONE" h)))
   ((fresh [t h1 h0]
      (== (llist h0 h1 t) l)
      (trace-lvars "foo" h0 h1 t)
      (!= h0 h1)
      (mydistincto (lcons h0 t))
      (mydistincto (lcons h1 t))))))


(defn member? [a l]
  (cond
    (empty? l) nil
    (= a (first l)) l
    :else (member? a (rest l))))

(filter #(not= :a %) [:a :b :a :a :a])

(defn some-distinct? [l]
  (cond
    (empty? l)  nil
    :else (let [[h & r] l]
            (not (empty? (filter #(not= h %) r))))))

(some-distinct? [:a :a :a :b :a])



(defn some-distincto [l]
  (conde
   ;; ((fresh [] (== () l) u#))
    ;; ((fresh [h] (== [h] l) u#))
   ((fresh [a b]
      (!= a b)
      (== [a b] l)
      (trace-lvars "TWO" a b l)
      ))                        ;  success!
   ((fresh [t h1 h0]
      ;; More than 2 entries:
      (== (llist h0 h1 t) l)
      (trace-lvars "foo" h0 h1 t l)
      ;; Try it on each possible sublist.
      (some-distincto (lcons h0 t))
      (some-distincto (lcons h1 t))))))


(defn not-all-equal?
  "A relation testing if s contains any distinct elements"
  [s]
  (run 1 [q]
    (fresh [a b r r2 perms]
      (conso a r suites)
      (membero b r)
      (!= a b)
      (== q [a b]))))


;;; How to generate every inner subpair?
;;; e.g. result here should be
;;; [ [spades clubs] [clubs hearts] [hearts diamonds] [diamonds clubs] ]
(let [suites [:spades :clubs :hearts :diamonds :clubs]]
  (run* [pair]
    (fresh [a b r]
;      (== pair [a b])
      (appendo pair r suites)
      )))

(defn some-distincto [l]
  (fresh [h t]
    (conso h t l)
    (trace-lvars "DIST" h t l)
    (conda
     [(everyg #(== h %) t) u#]
     [s# s#])))

(let [suites
      ;; [:spades :clubs :hearts :diamonds :clubs]
      ;; [:spades :spades :spades :spades :spades]
      ;; [:spades :spades :spades :spades :heart]
      [:spades :spades :spades :spades ]
      ]
  (run 5 [r s t u v]
    (== suites [r s t u])
    (membero v [:clubs :hearts])
    (some-distincto [v r s t u])
    ))

(run* [x y]
  (conde
   [(== x 'olive) s#]
   [(== y 'oil) s#]
   [s# u#]))

(defn anyo [g]
  (conde
   [g s#]
   [s# (anyo g)]))

(run 5 [q]
  (fresh [g]
    (anyo s#)
    (== g :foo)))
