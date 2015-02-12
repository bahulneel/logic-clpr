(ns logic-clpr.signed
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]))

(defn i->s
  [i]
  (if (< i 0)
    [1 (- i)]
    [0 i]))

(defn s->i
  [[s m]]
  (if (= s 0)
    m
    (- m)))

(defn boolc
  [b]
  (fd/in b (fd/domain 0 1)))

(defn naturalc
  [n]
  (fd/in n (fd/interval 0 (int (Math/pow 2 23)))))

(defn signedno
  [s m n]
  (l/fresh [s' m']
    (boolc s)
    (naturalc m)
    (boolc s')
    (naturalc m')
    (fd/eq (= s' s)
           (= m' m))
    (l/== [s' m'] n)))

(defn integer
  [s i]
  (l/fresh [ss sm]
    (signedno ss sm s)
    (l/project [ss sm i]
               (if-not (l/lvar? i)
                 (l/== [ss sm] (i->s i))
                 (if-not (or (l/lvar? ss) (l/lvar? sm))
                   (l/== i (s->i [ss sm]))
                   l/fail)))))

(defn addo
  [x y z]
  (l/fresh [xs xm
            ys ym
            zs zm]
    (signedno xs xm x)
    (signedno ys ym y)
    (signedno zs zm z)
    (l/conde [(fd/eq (= xs ys)
                     (= xs zs)
                     (= zm (+ xm ym)))]
             [(fd/eq (!= xs ys)
                     (= xs 0))
              (l/conde [(fd/eq (>= xm ym)
                               (= zs 0)
                               (= xm (+ ym zm)))]
                       [(fd/eq (< xm ym)
                               (= zs 1)
                               (= ym (+ xm zm)))])]
             [(fd/eq (!= xs ys)
                     (= xs 1))
              (addo y x z)])))
(defn subo
  [x y z]
  (addo z y x))

(defn multo
  [x y z]
  (l/fresh [xs xm
            ys ym
            zs zm]
    (signedno xs xm x)
    (signedno ys ym y)
    (signedno zs zm z)
    (fd/eq
     (<= xm zm)
     (<= ym zm)
     (= zm (* xm ym)))
    (l/conde [(fd/eq (= xs ys)
                     (= xs 0)
                     (= zs 0))]
             [(fd/eq (= xs ys)
                     (= xs 1)
                     (= zs 0))]
             [(fd/eq (= 1 xs)
                     (= 0 ys)
                     (= zs 1))]
             [(fd/eq (= 0 xs)
                     (= 1 ys)
                     (= zs 1))])))

(defn divo
  [x y z]
  (l/fresh [ys ym]
    (signedno ys ym y)
    (fd/!= 0 ym)
    (multo z y x)))
