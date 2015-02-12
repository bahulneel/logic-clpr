(ns logic-clpr.float
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]
            [logic-clpr.signed :as sn]
            [clojure.math.numeric-tower :as math]))

(defn r->f
  ([r]
     (r->f r 0))
  ([m e]
     (let [m' (int m)]
       (if (== m' (float m))
         (let [m'' (/ m' 10)]
           (if (and (pos? m'') (integer? m''))
             (recur m'' (inc e))
             [(sn/i->s m') (sn/i->s e)]))
         (recur (* m 10) (dec e))))))

(defn mantissa
  [[m e]]
  (sn/s->i m))

(defn exponent
  [[m e]]
  (sn/s->i e))

(defn f->r
  [f]
  (let [[m e] (map sn/s->i f)]
    (double (* m (math/expt 10 e)))))

(defn floato
  [m e f]
  (l/fresh [ms mm
            es em]
    (sn/signedno ms mm m)
    (sn/signedno es em e)
    (l/== [m e] f)))

(defn rshifto
  ([f1 f2 f1' f2']
     (l/fresh [m1 e1
               m2 e2]
       (floato m1 e1 f1)
       (floato m2 e2 f2)
       (l/conde [(sn/equalo e1 e2) (l/== f1 f1') (l/== f2 f2')]
                [(sn/lto e1 e2) (l/fresh [m2-shift
                                          e2-shift
                                          m e]
                                  (sn/subo e2 e1 e)
                                  (sn/expo (sn/i->s 10) e m)
                                  (sn/multo m2 m m2-shift)
                                  (sn/subo e2 e e2-shift)
                                  (floato m2-shift e2-shift f2')
                                  (l/== f1 f1'))]
                [(sn/gto e1 e2) (rshifto f2 f1 f2' f1')])))
  ([x' y' z' x y z]
     (l/fresh [xm xe
               ym ye
               zm ze]
       (floato xm xe x')
       (floato ym ye y')
       (floato zm ze z')
       (l/conde [(sn/equalo xe ye)
                 (sn/equalo xe ze)
                 (l/== x x')
                 (l/== y y')
                 (l/== z z')]
                [(sn/lto xe ye)
                 (sn/lto xe ze)
                 (rshifto x' y' x y)
                 (rshifto x' z' x z)]
                [(sn/lto ye xe)
                 (sn/lto ye xe)
                 (rshifto y' x' y x)
                 (rshifto y' z' y z)]
                [(sn/lto ze ye)
                 (sn/lto ze xe)
                 (rshifto z' y' z y)
                 (rshifto z' x' z x)]))))

(defn addo
  [x' y' z']
  (l/fresh [x y z]
    (rshifto x' y' z' x y z)
    (l/fresh [xm xe
              ym ye
              zm ze]
      (floato xm xe x)
      (floato ym ye y)
      (floato zm ze z)
      (sn/addo xm ym zm))))

(defn subo
  [x y z]
  (l/all
   (addo z y x)))

(defn multo
  [x y z]
  (l/fresh [xm xe
            ym ye
            zm ze]
    (floato xm xe x)
    (floato ym ye y)
    (floato zm ze z)
    (sn/multo xm ym zm)
    (sn/addo xe ye ze)))
