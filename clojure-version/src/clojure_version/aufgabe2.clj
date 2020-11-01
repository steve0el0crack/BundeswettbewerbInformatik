(ns clojure-version.aufgabe2
  (:require [clojure.java.io :as io]))

;; AUFGABE 2: Dreieckspuzzle

(defn Teilen []
  (let [file (with-open [rdr (clojure.java.io/reader (io/resource "clojure_version/aufgabe2sample2.txt"))]
               (reduce conj [] (line-seq rdr)))
       parts (rest (rest file))] 
    (map (fn [triple]
           (map #(read-string %1)
                (clojure.string/split triple #" ")))
         parts)))


;; Die Implementation geht um die Suche einer bestimmer Subgraph in einem groesseren Ksub9 Graph (fully connected graph mit 9 Knoten). Die Knoten stellen die Teilen ([x y z]) und die Kanten wuerden die moegliche Verbindungen inzwischen 2 Kanten: Maximal koennen 2 Dreicke in 3 verschiedene Weise verbunden werden, weil sie nur 3 Seiten haben und keine zweite Seite kann mit demsselben Dreieck verbunden. Das bedeutet Wir suchen nach 18 besondere Kanten, die durch alle 9 Knoten durchlaufen! In andere Woerter, geht dieses Problem um die komplette Suche eines Graphs. Dafuer kann man schlaue Tricks verwenden um die Suche zu reduzieren, wie zum Beispiel dass wenn es keine Knoten gibt, die mit anderen 3 verbunden werden koennte; dann kann man aus den Teilen keine grosseres Dreieck machen.
;; Um das Prozess besser zu erklaeren wird ein vereinfachtes Version des Problem "geloest": 4 kleine Dreiecke (aufgabe2sample2.txt).

(def parts (Teilen))

;; This is the criteria for stablishing/creating "edges" in our graph.
;; {n #{a b c}} means there are n paths between X and Y, each one formed by the union of a, or b, or c.
(defn beziehung
  [a b]
  (let [paths (clojure.set/intersection (set a) (set b))]
    {[a b] paths}))

(defn k-graph
  [nodes]
 (loop [p nodes
         e []]
    (if (= (count p) 1)
      (flatten e)
      (recur (rest p)
             (conj e (for [i (rest p)] (beziehung (first p) i)))))))

(k-graph parts)







