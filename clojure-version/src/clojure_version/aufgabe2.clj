(ns clojure-version.aufgabe2
  (:require [clojure.java.io :as io]))

;; AUFGABE 2: Dreieckspuzzle

(def Teilen 
  (let [file (with-open [rdr (clojure.java.io/reader (io/resource "clojure_version/aufgabe2sample.txt"))]
               (reduce conj [] (line-seq rdr)))]
    (map (fn [seq]
           (mapcat (fn [x]
                     (read-string x))
                   (clojure.string/split %1 #" ")
                   (rest (rest file)))))))

;; Die Implementation geht um die Suche einer bestimmer Subgraph in einem groesseren Ksub9 Graph (fully connected graph mit 9 Knoten). Die Knoten stellen die Teilen ([ai bi ci]) und die Kanten werden entweder den Wert 1 oder 0 haben: Die Kante zwischen zwei Punkte A [-1 2 3] und B [0 3 -2] wird den Wert 1 enthalten, weil die Seite "3" von A und B duerfen nebeneinander stehen. Wichtig ist zu merken, dass zwar 2 Knoten (Dreicke) mehr als 1 Seite in gemeinsam haben; duerfen wir nur ein Seiten-Paar "kleben".
;; Wir suchen nach 18 besondere Kanten.
