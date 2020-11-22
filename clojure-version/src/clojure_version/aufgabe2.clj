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
(def variance (read-string (first input)))
(def Teilen
  (let [parts (rest (rest input))] 
    (map (fn [triple]
           (map #(+ (read-string %1) variance)  
                (clojure.string/split triple #" ")))
         parts)))

;; In this example there is only ONE POSSIBLE SOLUTION.
;; the interval is [-7 | 7] -> [0 | 14] Therefore the goal is 14
;; The exercise is to find the middle triangle [-1 -3 -2]

(def mytest [[7 1 5] [2 4 6] [6 5 3] [-1 -3 -2]])

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
           (= (+ c1 x2) 14)
           (= (+ c2 x6) 14)
           (= (+ c3 x7) 14))
          
          (logic/== q {:A A
                       :B B
                       :C C
                       :details {D {A {:c1 c1
                                       :x2 x2}
                                    B {:x6 x6
                                       :c2 c2}
                                    C {:x7 x7
                                       :c3 c3}}}}))))))

(comment
  {:A (14 8 12),  ;; [7 1 5]
   :B (13 12 10), ;; [6 5 3]
   :C (9 11 13),   ;; [2 4 6]
   ;; D [7 1 5]
   :details {(6 4 5) {(14 8 12) {:c1 6,  ;; ... {-1 1}
                                 :x2 8},
                      (13 12 10) {:x6 10, ;; ... {3 -3}
                                  :c2 4},
                      (9 11 13) {:x7 9,   ;; ... {2 -2}
                                :c3 5}}}})

;; TEST PASSED! Solving for 4 pieces (finding the center)

;; In this test there is ONLY ONE POSSIBLE ANSWER.
;; Therefore, the time solving this one is the maximum time... and indeed IT IS VERY LONG TIME (>> 5m.)
(def naive-test
  [[10 10 2] [10 10 5] [10 10 7]     ;; A Ecken
   [-2 -3 -1] [-4 -5 -6] [-8 -9 -7]  ;; C Mitteln
   [10 1 8] [3 10 4] [9 6 10]])      ;; B Twice-connected

(def key-test
  [[-1 -1 -1] [1 1 1] [-1 -1 -1]
   [-1 -1 -1] [1 1 1] [-1 -1 -1]
   [-1 -1 -1] [1 1 1] [-1 -1 -1]])


(defn todo  []
  (logic/run 1 [q]
    (logic/fresh [n1 n2 n3
                  n4 n5 n6
                  n7 n8 n9
                  
                  C1 B1 A1
                  C2 B2 A2
                  C3 B3 A3]
      
      (logic/permuteo [C1 B1 A1
                       C2 B2 A2
                       C3 B3 A3]
                      (map #(map (fn [v] (+ v 1)) %1) key-test)) 

      (logic/permuteo C1 [n1 n2 n3])
      (logic/permuteo C2 [n4 n5 n6])
      (logic/permuteo C3 [n7 n8 n9])

      (logic/fresh [i1 i2 i3
                    i4 i5 i6
                    i7 i8 i9]


        (macro/symbol-macrolet [_ (logic/lvar)]
                               (logic/permuteo B1 [i1 i4 _])
                               (logic/permuteo B2 [i5 i8 _])
                               (logic/permuteo B3 [i2 i7 _])

                               (logic/permuteo A1 [i3 _ _])
                               (logic/permuteo A2 [i6 _ _])
                               (logic/permuteo A3 [i9 _ _]))

        (fd/in i1 i2 i3
               i4 i5 i6
               i7 i8 i9

               n1 n2 n3
               n4 n5 n6
               n7 n8 n9
               
               (fd/interval 0 2))

        ;; B1
        (logic/membero i1 B1)
        (logic/membero i4 B1)
        ;; B2
        (logic/membero i5 B2)
        (logic/membero i8 B2)
        ;; B3
        (logic/membero i2 B3)
        (logic/membero i7 B3)
        ;; A1
        (logic/membero i3 A1)
        
        (fd/eq
         ;; B1
         (= (+ n1 i1) 2) 
         (= (+ n4 i4) 2) 
         ;; B2
         (= (+ n5 i5) 2)
         (= (+ n8 i8) 2)
         ;; B3
         (= (+ n2 i2) 2)
         (= (+ n7 i7) 2)
         ;; A1
         (= (+ n3 i3) 2)
         ;; A2
         (= (+ 6 i6) 2)
         ;; A3
         (= (+ n9 i9) 2)
         )        
        
        (logic/== q {:c [C1 C2 C3]
                     :b [B1 B2 B3]
                     :a [A1 A2 A3]
                     
                     ;; the following structure should be read as:
                     :details {B1 {C1 {n1 i1}  ;; The connection between b1 and c1 is via {n1 i1}
                                   C2 {n4 i4}} ;; " "

                               B2 {C2 {n5 i5}  ;; Because n5 is in c2, and i5 in b2
                                   C3 {n8 i8}} ;; " "

                               B3 {C1 {n3 i3}
                                   C3 {n7 i7}}}})))))




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
