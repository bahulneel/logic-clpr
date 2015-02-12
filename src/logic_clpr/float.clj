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
       (if (== m' m)
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
  [f1 f2 f1' f2']
  (l/fresh [m1 e1
            m2 e2]
    (floato m1 e1 f1)
    (floato m2 e2 f2)
    (l/conde [(sn/equalo e1 e2) (l/== f1 f1') (l/== f2 f2')]
             [(sn/lto e1 e2) (l/fresh [f2-shift
                                       m2-shift
                                       e2-shift]
                               (sn/multo m2 (sn/i->s 10) m2-shift)
                               (sn/subo e2 (sn/i->s 1) e2-shift)
                               (floato m2-shift e2-shift f2-shift)
                               (rshifto f1 f2-shift f1' f2'))]
             [(sn/gto e1 e2) (rshifto f2 f1 f2' f1')])))

(defn addo
  [x' y' z]
  (l/fresh [x y]
    (rshifto x' y' x y)
    (l/fresh [xm xe
              ym ye
              zm ze]
      (floato xm xe x)
      (floato ym ye y)
      (floato zm ze z)
      (sn/equalo ze xe)
      (sn/addo xm ym zm))))
