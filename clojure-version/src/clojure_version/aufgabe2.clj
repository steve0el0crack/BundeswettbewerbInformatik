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

(def naive-test
  [[10 10 2] [10 10 5] [10 10 7]     ;; A Ecken
   [-2 -3 -1] [-4 -5 -6] [-8 -9 -7]  ;; C Mitteln
   [10 1 8] [3 10 4] [9 6 10]])      ;; B Twice-connected

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
                      (map #(map (fn [v] (+ v 10)) %1) naive-test)) 

      (logic/== C1 [n1 n2 n3])
      (logic/== C2 [n4 n5 n6])
      (logic/== C3 [n7 n8 n9])

      (logic/fresh [i1 i2 i3
                    i4 i5 i6
                    i7 i8 i9

                    x1 x2 x3
                    x4 x5 x6
                    x7 x8 x9]

        (logic/permuteo B1 [i1 i4 x1])
        (logic/permuteo B2 [i5 i8 x2])
        (logic/permuteo B3 [i2 i7 x3])

        (logic/permuteo A1 [i3 x4 x5])
        (logic/permuteo A2 [i6 x6 x7])
        (logic/permuteo A3 [i9 x8 x9])

        (fd/in i1 i2 i3
               i4 i5 i6
               i7 i8 i9

               n1 n2 n3
               n4 n5 n6
               n7 n8 n9

               x1 x2 x3
               x4 x5 x6
               x7 x8 x9
               
               (fd/interval 0 20))

        (fd/eq
         (= (+ n1 i1) 20))        
        
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

({:c [(20 20 12) (8 7 9) (20 11 18)],
  :b [(20 20 15) (6 5 4) (13 20 14)],
  :a [(20 20 17) (2 1 3) (19 16 20)],
  :details {(20 20 15) {(20 20 12) {20 20}, (8 7 9) {8 20}}, (6 5 4) {(8 7 9) {7 6}, (20 11 18) {11 5}}, (13 20 14) {(20 20 12) {12 20}, (20 11 18) {20 20}}}})

naive-test

(fd/eq
         ;; B1
         (= (+ n1 i1) 20)
         (= (+ n4 i4) 20)
         ;; B2
         (= (+ n5 i5) 20)
         (= (+ n8 i8) 20))

(todo)

(fd/in i1 i2 i3
               i4 i5 i6
               i7 i8 i9
               
               n1 n2 n3
               n4 n5 n6
               n7 n8 n9

               (fd/interval 0 (* 2 variance)))
        
        (fd/eq
         (= (+ i1 n1) (* 2 variance))
         (= (+ i4 n4) (* 2 variance))

         (= (+ i5 n5) (* 2 variance))
         (= (+ i8 n8) (* 2 variance))

         (= (+ i2 n2) (* 2 variance))
         (= (+ i7 n7) (* 2 variance))
         
         (= (+ i3 n3) (* 2 variance))

         (= (+ i6 n6) (* 2 variance))
         
         (= (+ i9 n9) (* 2 variance)))

({:c [(20 20 12) (8 7 9) (20 11 18)],
  :b [(20 20 15) (6 5 4) (13 20 14)],
  :a [(20 20 17) (2 1 3) (19 16 20)],
  
  :details {(20 20 15) {(20 20 12) {20 0},
                        (8 7 9) {8 12}},

            (6 5 4) {(8 7 9) {7 13},
                     (20 11 18) {11 9}},

            (13 20 14) {(20 20 12) {12 0},
                        (20 11 18) {20 0}}}})


;;ERROR 0
({:c [(2 1 4) (2 6 4) (5 5 2)],
  :b [(5 2 2) (5 0 6) (0 5 2)],
  :a [(2 1 5) (2 6 1) (1 4 0)],
  :details {(5 2 2) {(2 1 4) {2 4},
                     (2 6 4) {2 4}},
            
            (5 0 6) {(2 6 4) {6 0},
                     (5 5 2) {5 1}}, ;; these two two numbers add indeed up to 6, but they are NOT ON [5 0 6]! (n8 i8) | (5 1) ... b2 <> c2

            (0 5 2) {(2 1 4) {4 0},
                     (5 5 2) {5 0}}}})




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
