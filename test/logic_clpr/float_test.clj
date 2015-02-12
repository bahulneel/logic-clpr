(ns logic-clpr.float-test
  (:require [logic-clpr.float :refer :all]
            [midje.sweet :refer :all]
            [clojure.core.logic :refer [run fresh] :as l]
            [clojure.core.logic.fd :as fd]))

(facts "About Real to Float"
       (-> 0 r->f) => [[0 0] [0 0]]
       (-> 1 r->f) => [[0 1] [0 0]]
       (-> 101 r->f) => [[0 101] [0 0]]
       (-> 100 r->f) => [[0 1] [0 2]]
       (-> 1.0 r->f) => [[0 1] [0 0]]
       (-> -1.0 r->f) => [[1 1] [0 0]]
       (-> -1.1 r->f) => [[1 11] [1 1]]
       (-> -100.01 r->f) => [[1 10001] [1 2]])

(facts "About Float to Real"
       (-> 0 r->f f->r) => 0.0
       (-> 1 r->f f->r) => 1.0
       (-> 101 r->f f->r) => 101.0
       (-> 100 r->f f->r) => 100.0
       (-> 1.0 r->f f->r) => 1.0
       (-> -1.0 r->f f->r) => -1.0
       (-> -1.1 r->f f->r) => -1.1
       (-> -100.01 r->f f->r) => -100.01)

(facts "About floato"
       (fact "Given a float it passes"
             (let [f (r->f -1.1)]
               (->> (run 1 [q]
                      (fresh [e m]
                              (floato e m f)
                              (l/== q [e m])))
                    (map f->r)
                    first) => -1.1)))

(defn rshift
  [f1 f2]
  (first (run 1 [q]
           (fresh [f1' f2']
             (rshifto f1 f2 f1' f2')
             (l/== q [f1' f2'])))))

(facts "About radix shifting"
       (fact "If two numbers have the same radix we don't shift"
             (let [f1 (r->f 1)
                   f2 (r->f 2)
                   [f1' f2'] (rshift f1 f2)]
               f1' => f1
               f2' => f2))
       (fact "If the first number has a smaller radix we reduce the second"
             (let [f1 (r->f 1)
                   f2 (r->f 20)
                   [f1' f2'] (rshift f1 f2)]
               f1' => f1
               f2' =not=> f2
               (f->r f2') => (f->r f2)
               (exponent f2') => (exponent f1)))
       (fact "If the first number has a larger radix we reduce the first"
             (let [f1 (r->f -100)
                   f2 (r->f 0.2)
                   [f1' f2'] (rshift f1 f2)]
               f1' =not=> f1
               f2' => f2
               (f->r f1') => (f->r f1)
               (exponent f1') => (exponent f2))))


(defn do-op
  [op x' y']
  (let [x (r->f x')
        y (r->f y')]
    (->> (run 1 [z]
           (op x y z))
         (map f->r)
         first)))

(facts "About addition"
       (let [add (partial do-op addo)]
         (add 1 2) => 3.0
         (add 1 -2) => -1.0
         (add 1.1 2.2) => 3.3))
