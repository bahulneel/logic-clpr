(ns fixed
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]))

(defn fixedo
  [n r s f]
  (l/all
   (fd/in n (fd/interval 0 1))
   (fd/in s (fd/interval 0 10))
   (fd/in r (fd/interval 0 (Math/pow 10 10)))
   (l/== [n r s] f)))

(defn nato
  [i f]
  (fixedo 0 i 0 f))

(defn nego
  [f1 f2]
  (l/fresh [n r s]
    (fixedo n r s f1)
    (l/conde
     [(fd/== 0 n) (fixedo 1 r s f2)]
     [(fd/== 1 n) (fixedo 0 r s f2)])))

(defn eqo
  [f1 f2]
  (l/fresh [n1 r1 s1
            n2 r2 s2
            s']
    (fixedo n1 r1 s1 f1)
    (fixedo n2 r2 s2 f2)
    (fd/== n1 n2)
    (l/conde
     [(fd/eq (= s1 s2)
             (= r1 r2))]
     [(fd/eq (> s1 s2)
             )])))

(defn sumo
  [x y z]
  (l/fresh [x-n x-r x-s
            y-n y-r y-s
            z-n z-r z-s]
    (fixedo x-n x-r x-s x)
    (fixedo y-n y-r y-s y)
    (fixedo z-n z-r z-s z)
    (l/conde [(fd/eq
               (= x-s y-s)
               (= x-s z-s))
              (l/conde [(fd/eq
                         (= x-n y-n)
                         (= x-n z-n)
                         (= z-r (+ x-r y-r)))]
                       [(fd/eq
                         (= x-n 0)
                         (= y-n 1))
                        (l/conde [(fd/eq
                                   (> x-r y-r)
                                   (= 0 z-n)
                                   (= x-r (+ y-r z-r)))]
                                 [(fd/eq
                                   (> y-r x-r)
                                   (= 1 z-n)
                                   (= y-r (+ x-r z-r)))])]
                       [(fd/eq
                         (= x-n 1)
                         (= y-n 0))
                        (sumo y x z)])])))

(defn tst
  [n]
  (l/run n [q]
    (l/fresh [f1 f2]
      (fixedo 0 3 0 f1)
      (fixedo 0 30 1 f2)
      (eqo f1 f2))))

(defn n->s
  [n]
  (if (< n 0)
    [1 (- n)]
    [0 n]))

(defn s->n
  [[s m]]
  (if (= s 0)
    m
    (- m)))

(defn boolc
  [b]
  (fd/in b (fd/domain 0 1)))

(defn naturalc
  [n]
  (fd/in n (fd/domain 0 (Math/pow 2 23))))

(defn signedno
  [s m n]
  (l/all (boolc s)
         (naturalc m)

         (l/fresh [n-s n-m]
           (l/== [n-s n-m] n)
           (l/trace-lvars :signedo s m n-s n-m n)
           (fd/eq (= s n-s)
                  (= m n-m))
           (l/trace-lvars :signedo s m n-s n-m n))
         (l/trace-lvars :signedo s m n)))

(defn integero
  [s n]
  (l/project [s n]
             (if (l/lvar? s)
               (if (l/lvar? n)
                 l/succeed
                 (l/== s (n->s n)))
               (l/== n (s->n s)))))

(defn addo
  [x y z]
  (l/fresh [xs xm
            ys ym
            zs zm]
    (signedno xs xm x)
(l/trace-lvars :addo xs xm ys ym zs zm)
    (signedno ys ym y)
    (signedno zs zm z)

    (l/conde
     [(fd/eq (= xs ys)
             (= xs zs)
             (= zm (+ xs ys)))]
     )))

(defn tst2
  [n]
  (l/run n [q]
    (l/fresh [x]
      (integero [0 1] x)
      (integero q x))))

(defn tst3
  [n]
  (l/run n [q]
    (l/fresh [x y z]
      (integero x 2)
      (integero y 3)
      (addo x y z)
      (integero x q))))
