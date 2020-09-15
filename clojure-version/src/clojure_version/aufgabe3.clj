(ns Clojure-version.aufgabe3
  (:require [clojure.java.io :as io]))

(def levels
  (let [input (with-open [rdr (clojure.java.io/reader (io/resource "clojure_version/aufgabe3sample.txt"))]
                (reduce conj [] (line-seq rdr)))]
    (->> input
         (map #(read-string %1))
         rest)))

(defn liga
  [coll]
  (loop [c coll
         o []]
    (if (= (count c) 1)
      (flatten o)
      (recur (rest c)
             (conj o (map #(rand-nth [(first c) %1]) (rest c)))))))  ;; Bisher ist alles rein zufaellig. Die Funktion "rand-nth" wird naher geaendert, indem wir eine "Deviation" jenachdem welche Staerke wir einsetzen.

(def winner
  (let [tabelle (frequencies (liga levels))
        max (last (sort (vals tabelle)))
        winners (map first (filter #(= (last %1) max) tabelle))] 
    (if (= (count winners) 1)
      (first winners)
      (first (sort winners)))))  ;; Was passiert wenn ich 2 Spieler mit derselben Staerke kriege und die 2 bleiben an Unentschieden am Ende... wer gewinnt? Zahlen die 2 Spieler als dersselbe?


(def players (atom levels))

(defn steal
  [pool]
  (let [x (rand-nth @pool)]
    (swap! pool (fn [v]
               (remove #(= %1 x) v)))
    x))

(defn ko
  [round]
  (letfn [(pairing [pool]
            (map (fn [_]
                   (repeatedly 2 #(steal pool)))
                 (range (/ (count @pool) 2))))
          (play [pairs]
            (map #(rand-nth %1) pairs))]
    (let [round-winners (play (pairing round))]
      (if (= (count round-winners) 1)
        round-winners
        (recur (atom round-winners))))))

(ko players)

