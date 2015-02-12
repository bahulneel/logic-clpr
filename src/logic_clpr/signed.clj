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
  (fd/in n (fd/interval 0 (Math/pow 2 23))))

(defn signedno
  [s m n]
  (l/all (boolc s)
         (naturalc m)
         (l/== [s m] n)))

(defn integer
  [s i]
  (l/project [s i]
             (if (l/lvar? s)
               (if (l/lvar? i)
                 l/succeed
                 (l/== s (i->s i)))
               (l/== i (s->i s)))))

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
