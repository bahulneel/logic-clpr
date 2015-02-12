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

(facts "About the integer"
       (fact "For definate i"
             (first (run 1 [q] (integer q 1))) => [0 1]
             (first (run 1 [q] (integer q -11))) => [1 11])
       (fact "For definite s"
             (first (run 1 [q] (integer [0 1] q))) => 1
             (first (run 1 [q] (integer [1 11] q))) => -11)
       (fact "For unknown i,s we can do nothing"
             (first (run 1 [i] (fresh [s] (integer s i)))) => nil?)
       (fact "it is transative"
             (first (run 1 [q]
                      (fresh [x]
                        (integer x 123)
                        (integer x q)))) => 123
             (first (run 1 [q]
                      (fresh [x]
                        (integer [1 123] x)
                        (integer q x)))) => [1 123]))

(facts "About signedno"
       (fact "We can unpack an integer"
             (first (run 1 [q]
                      (fresh [s m n]
                        (integer n 1)
                        (signedno s m n)
                        (l/== q [s m])))) => [0 1])
       (fact "We can unpack a signed"
             (first (run 1 [q]
                      (fresh [n i]
                        (signedno 1 1 n)
                        (integer n i)
                        (l/== q i)))) => -1))

(defn do-op
  [op x y]
  (first (run 1 [z]
           (fresh [x' y' z']
             (integer x' x)
             (integer y' y)
             (op x' y' z')
             (integer z' z)))))

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
                          (integer x' -1)
                          (integer z' 3)
                          (addo x' y' z')
                          (integer y' y)))) => 4)))

(facts "About subtraction"
       (let [sub (partial do-op subo)]
         (sub 3 1) => 2
         (sub 1 3) => -2
         (sub -1 3) => -4
         (sub -1 -3) => 2))

(facts about "Multiplication"
       (let [mult (partial do-op multo)]
         (fact "Multiply by 1 leaves the original"
               (mult 1 1) => 1
               (mult 1 2) => 2
               (mult 2 1) => 2)
         (fact "Multiply by -1 changes the sign"
               (mult 123 -1) => -123
               (mult -1 51) => -51
               (mult -1 -3) => 3)))

(facts about "Division"
       (let [div (partial do-op divo)]
         (fact "Divide by 1 leaves the original"
               (div 1 1) => 1
               (div 3 1) => 3
               (div 27 1) => 27)
         (fact "Divide by -1 changes the sign"
               (div 1 -1) => -1
               (div 3 -1) => -3
               (div 27 -1) => -27)
         (fact "Divide by zero fails"
               (div 1 0) => nil)
         (fact "Can divide a number by its factor"
               (div 4 2) => 2
               (div 27 3) => 9)
         (fact "A number divided by itself is 1"
               (div 4 4) => 1)
         (fact "Cannot divide by a non-factor"
               (div 4 3) => nil
               (div -4 -3) => nil
               (div -4 3) => nil
               (div 4 -3) => nil)))

(facts "About running mult backwards"
       (fact "Can get the square root"
               (run 2 [x]
                 (fresh [x2' x']
                   (integer x2' 4)
                   (multo x' x' x2')
                   (l/== x' x))) => [[0 2] [1 2]]))
