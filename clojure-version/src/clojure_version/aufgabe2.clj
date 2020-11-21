(ns clojure-version.aufgabe2  
  (:require [clojure.java.io :as io]
            [clojure.core.logic :as logic]
            [clojure.core.logic.fd :as fd]
            [clojure-version.aufgabe1 :as aufgabe1]
            [clojure.tools.macro :as macro]))

;; AUFGABE 2: Dreieckspuzzle

(def input
  (with-open [rdr (clojure.java.io/reader (io/resource "clojure_version/aufgabe2sample.txt"))]
             (reduce conj [] (line-seq rdr))))

;; Each number was incremented because fd/interval only works with positive numbers. At the end they will be substracted...
(def Teilen
  (let [parts (rest (rest input))] 
    (map (fn [triple]
           (map #(+ (read-string %1) (read-string (first input)))  
                (clojure.string/split triple #" ")))
         parts)))

Teilen

;; In this example there is only ONE POSSIBLE SOLUTION.
;; the interval is [-7 | 7] -> [0 | 14] Therefore the goal is 14
;; The exercise is to find the middle triangle [-1 -3 -2]

(def mytest [[7 1 8] [2 4 0] [6 5 3] [-1 -3 -2]])

(defn test-todo []
  (logic/run 1 [q]
    (logic/fresh [A B C D] 

      (logic/permuteo [A B C D] (map #(map (fn [v] (+ v 7)) %1) mytest))

      (logic/fresh [x1 x2 x3
                    x4 x5 x6
                    x7 x8 x9]

        (logic/permuteo [x1 x2 x3] A)
        (logic/permuteo [x4 x5 x6] B)
        (logic/permuteo [x7 x8 x9] C)

        (logic/fresh [c1 c2 c3]
          (logic/permuteo [c1 c2 c3] D)

          (fd/in x2 x6 x7
                 c1 c2 c3
                 (fd/interval 0 14))
          
          (fd/eq
           (= (- c1 7) (- (- x2 7)))
           (= (- c2 7) (- (- x6 7)))
           (= (- c3 7) (- (- x7 7))))

          (logic/== q {:A A
                       :B B
                       :C C
                       :details {D {A x2
                                    B x6
                                    C x7}}
                       :alphabet {"x1" x1
                                  "x2" x2
                                  "x3" x3
                                  "x4" x4
                                  "x5" x5
                                  "x6" x6
                                  "x7" x7
                                  "x8" x8
                                  "x9" x9
                                  "c1" c1
                                  "c2" c2
                                  "c3" c3}}))))))

;; Suppose we have the same problem but just with two-side figures and the interval is [-3 | 3]
;; We can overrite this interval with [0 | 6] (+ 3) in order to work with "fd/in" and "fd/interval"
;; It seems like there is only one way to work some arithmetic... declaring a strict constraint interval for the variables!

;; -3 -2 -1 +1 +2 +3 -> 0 1 2 4 5 6   The condition before was that both add to 0
;; (-1 | 1) -> (2 | 4)                Now it became that both numbers add to 6, since they all are positive.

(logic/run 1 [q]
  (logic/fresh [A B C]

    (logic/permuteo [A B C] [[0 2 4] [6 4 5] [4 2 2]])

    (logic/fresh [fa fb fc
                  sa sb sc
                  ta tb tc]

      (logic/== A [fa sa ta])
      (logic/== B [fb sb tb])
      (logic/== C [fc sc tc])

      (fd/in fa fb fc sa sb sc ta tb tc (fd/interval 0 6))
      
      (fd/eq
       (= (+ fa fb) 6)
       (= (+ sb sc) 6)
       (= (+ tc ta) 6))

      (logic/== q [A B C]))))


(def pa [4 5])

(logic/run 1 [q]
  (logic/fresh [a b]
    (logic/membero a pa)
    (logic/membero b pa)
    (fd/eq
     (= (+ a b) 9))
    (logic/== q [a b])))


(defn todo  []
  (logic/run 1 [q]
    (logic/fresh [n1 n2 n3
                  n4 n5 n6
                  n7 n8 n9
                  
                  c1 b1 a1
                  c2 b2 a2
                  c3 b3 a3]
      
      (logic/permuteo [c1 b1 a1
                       c2 b2 a2
                       c3 b3 a3] Teilen) 

      (logic/== c1 [n1 n2 n3])
      (logic/== c2 [n4 n5 n6])
      (logic/== c3 [n7 n8 n9])

      (logic/fresh [i1 i2 i3
                    i4 i5 i6
                    i7 i8 i9

                    x1 x2 x3
                    x4 x5 x6
                    x7 x8 x9]

        (logic/permuteo b1 [i1 i4 x1])
        (logic/permuteo b2 [i5 i8 x2])
        (logic/permuteo b3 [i2 i7 x3])

        (logic/permuteo a1 [i3 x4 x5])
        (logic/permuteo a2 [i6 x6 x7])
        (logic/permuteo a3 [i9 x8 x9])

        (fd/in i1 i2 i3
               i4 i5 i6
               i7 i8 i9
               n1 n2 n3
               n4 n5 n6
               n7 n8 n9
               (fd/interval 1(* 2 (read-string (first input)))))
        
        (fd/eq
         (= (+ i1 n1) 0)
         (= (+ i4 n4) 0)

         (= (+ i5 n5) 0)
         (= (+ i8 n8) 0)

         (= (+ i2 n2) 0)
         (= (+ i7 n7) 0)
         
         (= (+ i3 n3) 0)

         (= (+ i6 n6) 0)
         
         (= (+ i9 n9) 0)))
      
      (logic/== q {:c [c1 c2 c3]
                   :b [b1 b2 b3]
                   :a [a1 a2 a3]
                   :details {b1 {c1 n1
                                 c2 n4}
                             b2 {c2 n5
                                 c3 n8}
                             b3 {c1 n3
                                 c3 n7}}
                   :alphabet {"n1" n1
                              "n2" n2
                              "n3" n3
                              "n4" n4
                              "n5" n5
                              "n6" n6
                              "n7" n7
                              "n8" n8
                              "n9" n9}}))))

(todo)

(logic/run 1 [q]
  (logic/fresh [x y]
    (logic/membero x [1 2 3])
    (logic/membero y [-2 2 -3])
    (fd/eq
     (= (+ x y) 0)
     (= (+ x y) 0))
    (logic/== q [x y])))

(logic.arithmetic/s one)

;; *************************** Implementation of peano Axiomen from (https://github.com/frenchy64/Logic-Starter/blob/master/src/logic_introduction/numbers.clj) ****************************

(defn s [x y] (logic/conso x [] y))

(first (logic/run 1 [q] (s (logic/fresh [a] (s 0 a)) q)))

(defn peano [n] (if (= n 0) 0 (cons (peano (- n 1)) nil)))

(defn natural-number 
  "A relation where x is a natural number"
  [x]
  (logic/conde
    [(== zero x)]
    [(logic/fresh [p]
       (s p x)
       (natural-number p))]))

(defn plus
  [x y z]
  (logic/conde
    [(logic/fresh [a]
       (== [zero a a] [x y z])
       (natural-number a))]
    [(logic/fresh [xp zp]
       (s xp x)
       (s zp z)
       (plus xp y zp))]))

(first (logic/run 1 [q]
    (logic/== q 4)))
