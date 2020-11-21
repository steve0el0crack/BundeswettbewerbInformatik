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
           (map #(read-string %1)  
                (clojure.string/split triple #" ")))
         parts)))

(def mytest [[7 1 8] [2 4 0] [6 5 3] [-1 -3 -2]])

(defn test-todo []
  (logic/run 1 [q]
    (logic/fresh [A B C D] ;; the kleine triangles
      (logic/permuteo [A B C D] mytest)
      (logic/fresh [x1 x2 x3
                    x4 x5 x6
                    x7 x8 x9]

        (logic/permuteo [x1 x2 x3] A)
        (logic/permuteo [x4 x5 x6] B)
        (logic/permuteo [x7 x8 x9] C)

        (logic/fresh [c1 c2 c3]
          (logic/permuteo [c1 c2 c3] D)

          (fd/eq
           (< (+ c1 0) 0)
           (< c2 0)
           (< c3 0))
          
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
                                  "c1" x2
                                  "c2" x6
                                  "c3" x7}}))))))

(test-todo)

(fd/eq
           (= (+ c1 x2) 0)
           (= (+ c2 x6) 0)
           (= (+ c3 x7) 0))

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




;; *************************** ANLYZING ****************************

