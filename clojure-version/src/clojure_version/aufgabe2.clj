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

(last
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

      (macro/symbol-macrolet [_ (logic/lvar)]
                             (logic/== a1 [n3 _ _])
                             (logic/== a2 [n6 _ _])
                             (logic/== a3 [n9 _ _]))

      (macro/symbol-macrolet [_ (logic/lvar)]
                             (logic/== b1 [n1 n4 _])
                             (logic/== b2 [n5 n8 _])
                             (logic/== b3 [n2 n7 _]))
      
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

;; *************************** ANLYZING ****************************

;; So there is the same error a lot of times...

;; with "permuteo" constrainting just B's
{:c [(5 0 6) (2 1 5) (5 5 2)],
 :b [(2 1 4) (2 6 4) (0 5 2)],
 :a [(5 2 2) (2 6 1) (1 4 0)],
 :details {(2 1 4) {(5 0 6) 5,
                    (2 1 5) 2},
           (2 6 4) {(2 1 5) 1,  ;; "False connection"... [2 6 4] cannot connect to [2 1 5] on port 1
                    (5 5 2) 5},
           (0 5 2) {(5 0 6) 6,
                    (5 5 2) 5}},
 :rest {"n2" 0,
        "n6" 5,
        "n9" 2}}

;; Replacing "permuteo" with "unifying" (==)
{:c [(5 0 6) (2 1 5) (5 5 2)],
 :b [(5 2 2) (2 6 4) (0 5 2)],
 :a [(2 1 4) (2 6 1) (1 4 0)],
 :details {(5 2 2) {(5 0 6) 5,
                    (2 1 5) 2},
           (2 6 4) {(2 1 5) 1,  ;; "False connection"... [2 6 4] cannot connect to [2 1 5] on port 1
                    (5 5 2) 5},
           (0 5 2) {(5 0 6) 6,
                    (5 5 2) 5}},
 :rest {"n2" 0,
        "n6" 5,
        "n9" 2}}

{:c [(5 0 6) (5 2 2) (5 5 2)],
 :b [(2 1 4) (2 6 4) (0 5 2)],
 :a [(2 1 5) (2 6 1) (1 4 0)],
 :details {(2 1 4) {(5 0 6) 5,   ;; "False connection"... [2 1 4] cannot connect to [5 0 6] on port 5
                    (5 2 2) 5},
           (2 6 4) {(5 2 2) 2,
                    (5 5 2) 5},  ;; "False connection"... [2 6 4] cannot connect to [5 5 2] on port 5
           (0 5 2) {(5 0 6) 6,   ;; "False connection"... [0 5 2] cannot connect to [5 0 6] on port 6
                    (5 5 2) 5}}, 
 :rest {"n2" 0,
        "n6" 2,
        "n9" 2}}

;; Replacing == with /permuteo for constraints both on A's and B's
{:c [(5 5 2) (2 1 5) (2 6 1)],
 :b [(2 1 4) (2 6 4) (0 5 2)],
 :a [(5 2 2) (5 0 6) (1 4 0)],
 :details {(2 1 4) {(5 5 2) 5,  ;; "False connection"... [2 1 4] cannot connect to [5 0 6] on port 5
                    (2 1 5) 2}, 
           (2 6 4) {(2 1 5) 1,  ;; "False connection"... [2 1 4] cannot connect to [5 0 6] on port 5
                    (2 6 1) 6},
           (0 5 2) {(5 5 2) 2,   ;; ...
                    (2 6 1) 2}}, ;; "False connection"... [0 5 2] cannot connect to [5 5 2] and [2 6 1] on port 2 at the same time!
 :rest {"n2" 5,
        "n5" 1,
        "n8" 6,
        "n6" 5,
        "n1" 5,
        "n4" 2,
        "n9" 1,
        "n3" 2,
        "n7" 2}}

;; == and constraints both for A's and B's
{:c [(5 5 2) (2 6 4) (2 6 1)],
 :b [(0 5 2) (5 0 6) (5 2 2)],
 :a [(2 1 5) (2 1 4) (1 4 0)],
 :details {(0 5 2) {(5 5 2) 5,  
                    (2 6 4) 2},
           (5 0 6) {(2 6 4) 6,   ;; "False connection"... [5 0 6] cannot connect to [2 6 4] and [2 6 1] on port 6 at the same time! 
                    (2 6 1) 6},  ;; ...
           (5 2 2) {(5 5 2) 2,
                    (2 6 1) 2}},
 :alphabet {"n1" 5,
            "n2" 5,
            "n3" 2,
            "n4" 2,
            "n5" 6,
            "n6" 4,
            "n7" 2,
            "n8" 6,
            "n9" 1}}

;; Constraints under one macro scope...
{:c [(5 0 6) (2 1 5) (5 5 2)],
 :b [(2 1 4) (2 6 4) (0 5 2)],
 :a [(5 2 2) (2 6 1) (1 4 0)],
 :details {(2 1 4) {(5 0 6) 5,
                    (2 1 5) 2},
           (2 6 4) {(2 1 5) 1,
                    (5 5 2) 5},
           (0 5 2) {(5 0 6) 6,
                    (5 5 2) 5}},
 :alphabet {"n1" 5,
            "n2" 0,
            "n3" 6,
            "n4" 2,
            "n5" 1,
            "n6" 5,
            "n7" 5,
            "n8" 5,
            "n9" 2}}

;; Bis now the best result was with "== and constraints for both A's and B's"... I am going to search deeper that section...
{:c [(5 5 2) (2 6 4) (2 6 1)],
 :b [(0 5 2) (5 0 6) (5 2 2)],
 :a [(2 1 5) (2 1 4) (1 4 0)],
 :details {(0 5 2) {(5 5 2) 5,
                    (2 6 4) 2},
           (5 0 6) {(2 6 4) 6,
                    (2 6 1) 6},
           (5 2 2) {(5 5 2) 2,
                    (2 6 1) 2}},
 :alphabet {"n1" 5,
            "n2" 5,
            "n3" 2,
            "n4" 2,
            "n5" 6,
            "n6" 4,
            "n7" 2,
            "n8" 6,
            "n9" 1}}


