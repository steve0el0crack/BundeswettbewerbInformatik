(ns clojure-version.aufgabe2  
  (:require [clojure.java.io :as io]
            [clojure.core.logic :as logic]
            [clojure.core.logic.fd :as fd]
            [clojure-version.aufgabe1 :as aufgabe1]
            [clojure.tools.macro :as macro]))

;; AUFGABE 2: Dreieckspuzzle

(def Teilen
  (let [file (with-open [rdr (clojure.java.io/reader (io/resource "clojure_version/aufgabe2sample2.txt"))]
               (reduce conj [] (line-seq rdr)))
       parts (rest (rest file))] 
    (map (fn [triple]
           (map #(read-string %1)
                (clojure.string/split triple #" ")))
         parts)))

(defn Superset
  [coll]
  (map #(remove (fn [_] (= _ nil)) %1) (aufgabe1/combinatorics (map (fn [e] [e nil]) coll))))

(logic/defne not-membero
  [x l]
  ([_ []]) 
  ([_ [?y . ?r]]
   (logic/!= x ?y) 
   (not-membero x ?r)))

(defn paolo
  [coll1 coll2]
  (logic/fresh [a b] 
    (logic/conde
     [(logic/membero a coll1) (not-membero a coll2)]
     [(logic/membero b coll2) (not-membero b coll1)])))

(defn connects?
  [d1 d2]
  (let [cc (logic/run* [A B]
             (logic/membero A (Superset d1))
             (logic/membero B (Superset d2))
             (logic/project [A B]
                            (logic/== (count A) (count B)))
             (logic/nafc paolo A B))]
    (reduce #(if (> (count %1) (count %2)) %1 %2) (map first cc))))


(def karte
  (letfn [(map-constructor
            [coll d]
            (reduce (fn [p n] (assoc p n (connects? d n)))
                    {}
                    coll))]
    (apply conj (map (fn [T]
                       {T (map-constructor (remove #(= %1 T) Teilen) T)})
                     Teilen))))
(comment
  {(2 1 0) {(0 1 0) (1 0),
            (2 0 1) (2 1 0),
            (0 2 2) (2 0)},
   (0 1 0) {(2 1 0) (1 0),
            (2 0 1) (1 0),
            (0 2 2) (0)},
   (2 0 1) {(2 1 0) (2 0 1),
            (0 1 0) (0 1),
            (0 2 2) (2 0)},
   (0 2 2) {(2 1 0) (0 2),
            (0 1 0) (0),
            (2 0 1) (0 2)}})

(defn get-conjunctions
  [d]
  (letfn [(foo [d]
            (comp
             (filter (fn [_] (= (first _) d)))
             (mapcat second)
             (map second)))]
    (transduce (foo d) conj [] karte)))

Teilen

((let [center (logic/lvar)
        bridges (repeatedly 3 logic/lvar)
        rest (remove #(= center %1) Teilen)]
    (logic/run* [q]
      (logic/membero center Teilen)
      (logic/permuteo bridges center)
      (logic/fresh [d1 d2 d3]
        (logic/membero d1 rest)
        (logic/membero d2 rest)
        (logic/membero d3 rest)
        (logic/!= d1 d2)
        (logic/!= d1 d3)
        (logic/!= d3 d2)
        (logic/fresh [a b c]
          (logic/== [a b c] bridges)
          (logic/membero a d1)
          (logic/membero b d2)
          (logic/membero c d3))
        (logic/== q [center bridges [d1 d2 d3]])))))



