(ns logic-clpr.signed-test
  (:require [logic-clpr.signed :refer :all]
            [midje.sweet :refer :all]
            [clojure.core.logic :refer [run fresh] :as l]
            [clojure.core.logic.fd :as fd]))

(facts "About Integer to Signed"
       (fact "Positive integers have 0 sign bit"
             (-> 0 i->s first) => 0
             (-> 1 i->s first) => 0
             (-> 101 i->s first) => 0)
       (fact "Negative integers have 1 sign bit"
             (-> -1 i->s first) => 1
             (-> -101 i->s first) => 1)
       (fact "All numbers have a mantissa equal to the madnitude"
             (-> 0 i->s second) => 0
             (-> 1 i->s second) => 1
             (-> -1 i->s second) => 1
             (-> 10 i->s second) => 10
             (-> -10 i->s second) => 10))

(facts "About Signed to Integer"
       (fact "Positive integers"
             (-> [0 0] s->i) => 0
             (-> [0 1] s->i) => 1
             (-> [0 101] s->i) => 101)
       (fact "Negative integers"
             (-> [1 1] s->i) => -1
             (-> [1 101] s->i) => -101))

(facts "About the integero"
       (fact "For definate i"
             (first (run 1 [q] (integero q 1))) => [0 1]
             (first (run 1 [q] (integero q -11))) => [1 11])
       (fact "For definite s"
             (first (run 1 [q] (integero [0 1] q))) => 1
             (first (run 1 [q] (integero [1 11] q))) => -11)
       (fact "For unknown i,s"
             (first (run 1 [s i] (integero s i))) => '[_0 _1])
       (fact "it is transative"
             (first (run 1 [q]
                      (fresh [x]
                        (integero x 123)
                        (integero x q)))) => 123
             (first (run 1 [q]
                      (fresh [x]
                        (integero [1 123] x)
                        (integero q x)))) => [1 123]))

(facts "About signedno"
       (fact "We can unpack an integer"
             (first (run 1 [q]
                      (fresh [s m n]
                        (integero n 1)
                        (signedno s m n)
                        (l/== q [s m])))) => [0 1])
       (fact "We can unpack a signed"
             (first (run 1 [q]
                      (fresh [n i]
                        (signedno 1 1 n)
                        (integero n i)
                        (l/== q i)))) => -1))

(defn do-op
  [op x y]
  (first (run 1 [z]
           (fresh [x' y' z']
             (integero x' x)
             (integero y' y)
             (op x' y' z')
             (integero z' z)))))

(facts "About addition"
       (let [add (partial do-op addo)]
         (fact "The sum of two positive integers is the sum of the mantissas"
               (add 2 3) => 5)
         (fact "The sum of two negative integers is the sum of the mantissas"
               (add -2 -3) => -5)
         (fact "The sum of a positive integer and a negative integer"
               (add 1 -1) => 0
               (add 3 -1) => 2
               (add 3 -5) => -2)
         (fact "The sum of a negative integer and a positive integer"
               (add -1 1) => 0
               (add -1 3) => 2
               (add -5 3) => -2)
         (fact "Can run backwards"
               (first (run 1 [y]
                        (fresh [x' y' z']
                          (integero x' -1)
                          (integero z' 3)
                          (addo x' y' z')
                          (integero y' y)))) => 4)))

(facts "About subtraction"
       (let [sub (partial do-op subo)]
         (sub 3 1) => 2
         (sub 1 3) => -2
         (sub -1 3) => -4
         (sub -1 -3) => 2))
