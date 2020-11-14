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

(logic/run 1 [q]
  (logic/fresh [n1 n2 n3
                n4 n5 n6
                n7 n8 n9]
    (logic/fresh [c1 b1 a1
                  c2 b2 a2
                  c3 b3 a3]
      (logic/permuteo [c1 b1 a1 c2 b2 a2 c3 b3 a3] Teilen)  ;; this breaks if I am given more pieces than I actually need, or less...

      (logic/permuteo c1 [n1 n2 n3])
      (logic/permuteo c2 [n4 n5 n6])
      (logic/permuteo c3 [n7 n8 n9])

      (macro/symbol-macrolet [_ (logic/lvar)]
       (logic/permuteo b1 [n1 n4 _])
       (logic/permuteo b2 [n5 n8 _])
       (logic/permuteo b3 [n2 n7 _]))

      (logic/== q {:c [c1 c2 c3]
                   :b [b1 b2 b3]
                   :a [a1 a2 a3]}))))

