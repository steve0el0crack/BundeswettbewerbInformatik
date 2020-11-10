(ns clojure-version.aufgabe2  
  (:require [clojure.java.io :as io]
            [clojure.core.logic :as logic]
            [clojure.core.logic.fd :as fd]
            [clojure-version.aufgabe1 :as aufgabe1]))

;; AUFGABE 2: Dreieckspuzzle

(def Teilen
  (let [file (with-open [rdr (clojure.java.io/reader (io/resource "clojure_version/aufgabe2sample2.txt"))]
               (reduce conj [] (line-seq rdr)))
       parts (rest (rest file))] 
    (map (fn [triple]
           (map #(read-string %1)
                (clojure.string/split triple #" ")))
         parts)))


;; Die Implementation geht um die Suche einer bestimmer Subgraph in einem groesseren Ksub9 Graph (fully connected graph mit 9 Knoten). Die Knoten stellen die Teilen ([x y z]) und die Kanten wuerden die moegliche Verbindungen inzwischen 2 Kanten: Maximal koennen 2 Dreicke in 3 verschiedene Weise verbunden werden, weil sie nur 3 Seiten haben und keine zweite Seite kann mit demsselben Dreieck verbunden. Das bedeutet Wir suchen nach 18 besondere Kanten, die durch alle 9 Knoten durchlaufen! In andere Woerter, geht dieses Problem um die komplette Suche eines Graphs. Dafuer kann man schlaue Tricks verwenden um die Suche zu reduzieren, wie zum Beispiel dass wenn es keine Knoten gibt, die mit anderen 3 verbunden werden koennte; dann kann man aus den Teilen keine grosseres Dreieck machen.
;; Um das Prozess besser zu erklaeren wird ein vereinfachtes Version des Problem "geloest": 4 kleine Dreiecke (aufgabe2sample2.txt).

(defn Superset
  [coll]
  (map #(remove (fn [_] (= _ nil)) %1) (aufgabe1/combinatorics (map (fn [e] [e nil]) coll))))

(logic/defne not-membero
  [x l]
  ([_ []]) ;; So, anything is NOT A MEMBER of []... nothing. Includin nothing itself, then follows that [] is contained within _ (bottom)
  ([_ [?y . ?r]]
   (logic/!= x ?y) ;; first checks fot the first element, and then continues recursively. If there is at least one element in the collection equal to the one given by us at the beginning of the process... it fails since it is a NESTED IF.
   (not-membero x ?r)))

(defn paolo
  [coll1 coll2]
  ; [[1 2] [2 3]] [[1 2] [3 2]] -> this would evaluate to a wrong answer since [2 3] [3 2] are considered 2 different groups.
  (logic/fresh [a b] ;; fresh return a function to be used in a RUN BLOCK!!!
    (logic/conde
     [(logic/membero a coll1) (not-membero a coll2)]
     [(logic/membero b coll2) (not-membero b coll1)])))

(defn connects?
  [d1 d2]
  (let [cc (logic/run* [A B]
             (logic/membero A (Superset d1))
             (logic/membero B (Superset d2))
             (logic/nafc paolo A B))]
    (reduce #(if (> (count %1) (count %2)) %1 %2) (map first cc))))

(def karte
  (letfn [(foo [coll d] (reduce (fn [p n] (assoc p n (connects? d n))) {} coll))]
    (apply conj (map (fn [T] {T (foo (remove #(= %1 T) Teilen) T)}) Teilen))))

(comment {(2 1 0) {(0 1 0) (1 0),
                   (2 0 1) (2 1 0),
                   (0 2 2) (2 0)},
          (0 1 0) {(2 1 0) (0 1 0),
                   (2 0 1) (0 1 0),
                   (0 2 2) (0 0)},
          (2 0 1) {(2 1 0) (2 0 1),
                   (0 1 0) (0 1),
                   (0 2 2) (2 0)},
          (0 2 2) {(2 1 0) (0 2 2),
                   (0 1 0) (0),
                   (2 0 1) (0 2 2)}})



