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
(def fixed-vars (map (fn [h] (filter #(= 1 (count (second %1))) h)) structure)) ;; needs more code to get to the pair (key value)

(defn hello
  [parameter] ;; this must be better explained!
  (let [pool (map second (first structure)) ;; I am looking into the first column
        cond1 (map second (second structure)) ;; then into the second one
        cond2 (map second (last structure)) ;; and finally into the last.
        vars (repeatedly (count pool) logic/lvar)] 
    (logic/run 1 [q]

      (rec-membero vars pool)

      (macro/symbol-macrolet [S (map #(remove (fn [_] (= _ nil)) %1) (Superset [] vars))]
                             (fd/<= (count (remove #(not-membero S %1) cond1)) parameter)  ;; this is for continuing trying with other values
                             (fd/<= (count (remove #(not-membero S %1) cond2)) parameter)) ;; " "
      
      (logic/== q vars))))

;; this is going to search for the coordinates (indexes) which should be marked in order to NOT BLOCK other unique variables...
(defn zeta [n]
  (if (empty? (hello n)) (zeta (+ n 1)) (hello n)))

;; Till now, we have find the following:
;; * fixed-vars -> Unique variables that must be marked on the first COLUMN, since they are nowhere else (the reasoning behind this procedure is that if they are not marked at THAT COLUMN and at THAT INDEX, then there are two possibilities: It is going to be marked on the one of the 2 next columns, which would give us at the end a WORSE RATING for the selection. The second possibility is that they are not going to be marked at all! That would mean that at the end of the distribution/assignment of gifts; one kid will receive this gift without even having wished for it... it would be completely random)
;; * hello -> Focusing on those vars which could be positioned at many different indexes, the question is which of these configurations to choose? With the help of a logic engine; one can find the configuration that leaves THE MOST/BETTER ROOM for other variables. The reasoning behind this is that sometimes choosing one of 2 possible coordinates for a determined variable WILL MAKE the difference when choosing the following.

;; In other words, till now we should have the following: "On the first COLUMN the numbers/variables/gifts [...] should be marked at the index's [...]". That means... the first row is complete! And the rest is just repeating the same, taking into account other "parameters"...

(logic/defne rec-membero
  [vars nested-coll]
  ([_ []])
  ([[x . xr] [S . rS]]
   (logic/membero x S)
   (rec-membero xr rS)))

(logic/defne rec-notmembero
  [l coll]
  ([_ []])
  ([_ [?x . ?r]]   
   (not-membero ?x l)
   (rec-notmembero l ?r)))

(logic/defne not-membero [x l]
  ([_ []])
  ([_ [?y . ?r]]
   (logic/fresh [h]  ;; when working with lists...
     (logic/permuteo h x)
     (logic/!= h ?y))
   (not-membero x ?r)))

(defn Superset
  [c set]
  (let [i (gensym)]
    (if (= (count set) 1)
      (map (fn [a] (conj c a)) [nil (first set)])
      (mapcat (fn [i] (Superset (conj c i) (rest set))) [nil (first set)]))))



