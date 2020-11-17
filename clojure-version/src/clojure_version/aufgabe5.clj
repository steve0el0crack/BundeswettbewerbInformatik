(ns clojure-version.aufgabe5
  (:require [clojure.java.io :as io]
            [clojure.core.logic :as logic]
            [clojure.core.logic.fd :as fd]
            [clojure-version.aufgabe1 :as aufgabe1]
            [clojure.tools.macro :as macro]))

;; AFGABE 5: Wichteln

(def input
  (with-open [rdr (clojure.java.io/reader (io/resource "clojure_version/aufgabe5sample.txt"))]
    (reduce conj [] (line-seq rdr))))

(def hey
  (comp
   (map #(clojure.string/split %1 #" "))
   (map (fn [_] (remove #(= %1 "") _)))
   (map #(map read-string %1))))

(def data (transduce hey conj [] (rest input)))

(logic/defne omniscient
  [a A]
  ([[] []])
  ([[x . rx] [y . ry]]
   (logic/membero x y)
   (subgoal rx ry)))

(let [answers (repeatedly (count data) logic/lvar)]
 (logic/run* [q]
   (logic/distincto answers)
   (omniscient answers
               data)
   (logic/== q answers)))

data

(let [answers (repeatedly 9 logic/lvar)]
 (logic/run 1 [q]
   (logic/distincto answers)
   (omniscient answers [(nth data 0)
                        (nth data 1)
                        (nth data 2)
                        (nth data 3)
                        (nth data 4)
                        (nth data 5)
                        (nth data 6)
                        (nth data 7)
                        (nth data 8)
                        ])
   (logic/== q answers)))
