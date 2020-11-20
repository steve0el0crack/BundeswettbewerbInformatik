(ns clojure-version.aufgabe5
  (:require [clojure.java.io :as io]
            [clojure.core.logic :as logic]
            [clojure.core.logic.fd :as fd]
            [clojure-version.aufgabe1 :as aufgabe1]
            [clojure.tools.macro :as macro]
            [clojure.set :as set]))

;; AFGABE 5: Wichteln

(def input
  (let [hey (comp
             (map #(clojure.string/split %1 #" "))
             (map (fn [_] (remove #(= %1 "") _)))
             (map #(map read-string %1)))
        input (with-open [rdr (clojure.java.io/reader (io/resource "clojure_version/aufgabe5sample2.txt"))]
                (reduce conj [] (line-seq rdr)))]
    (transduce hey conj [] input)))

(def data (rest input))

;; {:per-column [[...] [...] [...]] :f #{...} :s #{...} :l #{...}}

(def description
  (let [per-column (map #(map %1 data) [first second last])
        uniques (map set per-column)
        first-wish (first uniques)
        second-wish (set/difference (second uniques) (first uniques))
        third-wish   (set/difference (last uniques) (set/union (second uniques) (first uniques)))]
    {:per-column per-column :f first-wish :s second-wish  :l third-wish}))

(def structure
 (map (fn [f k]
         (apply conj (map (fn [v]
                            (let [enumerate (map-indexed (fn [i v] [i v]) (f (:per-column description)))]
                              {v (map first (filter #(= (second %1) v) enumerate))}))
                          (k description))))
       [first second last] [:f :s :l]))

;; v's are the unique numbers (they are all different) and the coll's are the positions at which they appear at :x column.
;; [{v1 [...], v2 [...], v3 [...]} -> :f irst column
;;  {va [...], vb [...]}           -> :s econd column
;;  {vx [...] ... }]               -> :l ast column

;; Is there any variable with just one possibility? It means if there is a number on any column that appears just once.
(def fixed-vars (map (fn [h] (filter #(= 1 (count (second %1))) h)) structure))

(let [pool (map second (first structure))  ;; I am looking into the first column
      cond1 (map second (second structure))  ;; then into the second one
      cond2 (map second (last structure))]  ;; and finally into the last.
  (logic/run 1 [q]
    (logic/fresh [a b c x] ;; a, b and c are not going to be the same number since it is impossible for two numbers to occupy the same index on an array...
      (logic/membero a (nth pool 0))
      (logic/membero b (nth pool 1))
      (logic/membero c (nth pool 2))

      (logic/permuteo x [a b c])
      (not-membero x cond1)
      (not-membero x cond2)

      (logic/== q [a b c]))))

(logic/defne rec-membero
  [vars nested-coll]
  ([_ []])
  ([[x . xr] [S . rS]]
   (logic/membero x S)
   (rec-membero xr rS)))

(logic/run 2 [a]
  (rec-membero [a] [[1 2]]))

(logic/defne not-membero [x l]
  ([_ []])
  ([_ [?y . ?r]]
    (logic/!= x ?y)
    (not-membero x ?r)))




