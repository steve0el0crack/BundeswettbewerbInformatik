(ns clojure-version.core
  (:require [clojure.java.io :as io]))

;; AUFGABE 1: Woerter aufraeumen 

(defn start []
  (let [file (with-open [rdr (clojure.java.io/reader (io/resource "clojure_version/example.txt"))]
               (reduce conj [] (line-seq rdr)))]
    (map (fn [string]
           (-> string
               (clojure.string/replace (re-pattern "[,;:.]") "") ;; every punctuation must be out of the analysis
               (clojure.string/split #" "))) ;; converting into single elements of a list
         file)))

;; Will contain two dictionaries (Hashmaps) in a list: The first for the incomplete words and the second for the complete ones.
;; This will be the structure: [{"___e" : 4, "__d" : 3, ...} {"eine" : 4, "und" : 3, ...}]
(def counted
  (map (fn [f]
         (apply hash-map
                (mapcat (fn [x]
                          [x (count x)]) ;; if otherwise (first the number of characters and then the word), the function hash-map would crash multiple values onto 1 key and words would be lost.
                        (f (start)))))
       [first second]))  ;; length of each string, for text and words as a hash-map.

;; Counting how many complete/incomplete words of length "x" there is.
;; [{7 : 2, 5 : 4, ...} {7 : 2, 5 : 4, ...}] (meaning that there is 2 incomplete words of length 7 and 4 of length 5. And 2 complete words of length 7, and 4 of length 5)
;; This could be the "decision procedure" for not going into any computation: If there is a missmatch in these numbers, then there will be no solution.
(def freq
  (map (fn [f]
         (frequencies (vals (f counted))))
       [first second]))  

;; In order to sort both kinds of words (complete and incomplete) under some common criteria: Length.
;; Starting to stablish relationships between the two groups.
;; #{ 1 2 3 4 5... Ni} such that any N(i) = N(j)  
(def unique-set
  (apply clojure.set/union
         (map (fn [f]
                (set (vals (f counted))))
              [first second])))  ;; In general, which are the sizes of my "strings" (text/words).

;; For any given "n" (possible length of a word), the words corresponding to this length are going to be "wrapped" out of their corresponding groups.
;; [["___e", "o__r"]["eine", "oder"]] (would be for example "(wrap 4)")
(defn wrap [n]
  (map (fn [f]
         (map first
              (filter #(= (last %1) n)
                      (f counted))))
       [first last]))

(apply hash-map (mapcat #(wrap %1) unique-set))
