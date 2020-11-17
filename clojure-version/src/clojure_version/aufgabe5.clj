(ns clojure-version.aufgabe5
  (:require [clojure.java.io :as io]
            [clojure.core.logic :as logic]
            [clojure.core.logic.fd :as fd]
            [clojure-version.aufgabe1 :as aufgabe1]
            [clojure.tools.macro :as macro]
            [clojure.set :as set]))

;; AFGABE 5: Wichteln

(def input
  (with-open [rdr (clojure.java.io/reader (io/resource "clojure_version/aufgabe5sample3.txt"))]
    (reduce conj [] (line-seq rdr))))

(def hey
  (comp
   (map #(clojure.string/split %1 #" "))
   (map (fn [_] (remove #(= %1 "") _)))
   (map #(map read-string %1))))

(def data (transduce hey conj [] (rest input)))

(def best-choice  ;; gives only the numbers which must be selected on each file (:f first :s second :l last).
 (let [per-file (map #(map %1 data) [first second last])
        uniques (map set per-file)
        first-wish (first uniques)
        second-wish (set/difference (second uniques) (first uniques))
        third-wish   (set/difference (last uniques) (set/union (second uniques) (first uniques)))]
   {:per-file per-file :f first-wish :s second-wish  :l third-wish}))

(defn pos-finding
  [coll0 row foo] ;; [data :f first] ... would be for the first search of places, where every row is valid.
  (loop [pool coll0
         labels (row best-choice)
         h []]
    (if (empty? labels)
      h
      (let [i (->> (foo (:per-file best-choice))  ;; I can take this out in a LET structure.
                   (map-indexed (fn [i v] [i v]))
                   (filter #(= (first labels) (second %1)))
                   (map first)
                   (rand-nth))]
        (recur (apply conj (subvec data 0 i) (subvec data i (- (count data) 1)))
               (rest labels)
               (conj h i))))))

(defn after-chose
  [coll0 key foo]  ;;on a list of possible (free) rows, it takes out every one which was already used...
  (->> coll0
       (map-indexed (fn [i v] [i v]))
       (remove (fn [coll] (some #(= %1 (first coll)) (pos-finding key foo))))
       (map second)))

(def first-row-positions {(pos-finding data :f first) (:f best-choice)})
{  [4 27 12 24 10 9 5 7 0 16 25 18 15 19 21]
 #{20 27 4 29 6 28 3 12 2 9 26 16 30 10 8}}

(def second-row-positions {(pos-finding (after-chose data :f first) :s second) (:s best-choice)})
{ [17 23 27 13 20 12 9]
 #{7 15 21 23 19 14 18}}

(def third-row-positions {(pos-finding (after-chose (after-chose data :f first) :s second) :l last) (:l best-choice)})
{[19 27] #{13 17}}




