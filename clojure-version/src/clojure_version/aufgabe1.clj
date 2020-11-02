(ns clojure-version.core 
  (:require [clojure.java.io :as io]
            [clojure.tools :as tools]))

;; AUFGABE 1: Woerter aufraeumen 

;; I think I could use some "transducer" for this process pipeline...
(def start
  (let [file (with-open [rdr (clojure.java.io/reader (io/resource "clojure_version/aufgabe1sample.txt"))]
               (reduce conj [] (line-seq rdr))) 
        [readed accent] (loop [i 0
                               pool (-> (first file)
                                        (clojure.string/split #" ")) ;; 20 elements...
                               readed []
                               accent []]
                          (if (= i (count (-> (first file)
                                              (clojure.string/split #" "))))
                            [readed accent]
                            (if (re-find #"[,;:.]" (first pool))
                              (recur (inc i)
                                     (rest pool)
                                     (conj readed (clojure.string/replace (first pool) #"[,;:.]" ""))
                                     (conj accent (first pool)))
                              (recur (inc i)
                                     (rest pool)
                                     (conj readed (first pool))
                                     accent))))]
    {:incompletes readed
     :completes (-> (second file)
                    (clojure.string/split #" "))
     :accents accent}))
start

(spy)

;; Will contain two dictionaries (Hashmaps) in a list: The first for the incomplete words and the second for the complete ones.
;; This will be the structure: [{"___e" : 4, "__d" : 3, ...} {"eine" : 4, "und" : 3, ...}]
  

(def first-filter
  (letfn [(label []
            (map (fn [f]
                   (apply hash-map (mapcat (fn [x] [x (count x)]) (f start))))
                 [:incompletes :completes]))
          (freq []
            (map (fn [f]
                   (frequencies (vals (f (label)))))
                 [first second]))
          (solvable? []
            (apply = (map (fn [a b] (= a b)) (first (freq)) (second (freq)))))
          (solved []  
            (if (solvable?)
              (filter (fn [x] (if (= (second x) 1))) (first freq)) ;; [[11 1] [9 1] [8 1]] and then must I go into each list again and extract the 11, 9 and 8?!
              nil))
          (unique-set []
            (apply clojure.set/union
                   (map (fn [f]
                          (set (vals (f labeled))))
                        [first second])))
          (wrap [n]
           (map (fn [f]
                   (map first
                        (filter #(= (last %1) n)
                                (f labeled))))
                [first second]))]
    (apply hash-map (mapcat #(wrap %1) (unique-set)))))

first-filter

(def coll {:a 1 :b 3 :c 4 :d 5})

(def xf
  (comp
   (filter #(= (second %1) 1))
   (map first)))

(transduce xf conj [] coll)

(def second-filtered (remove #(= (count (first %1)) 1) first-filtered))  ;; only cases in which there are various possibilities for a match.

(defn fitness
  [a b]
  (reduce + (map #(if (= %2 %1) 1 0) a b)))

(def test
  (mapcat (fn [a]
        (map (fn [i]
               (let [best (reduce-map
                           (apply hash-map
                                  (mapcat (fn [x]
                                            [x (fitness x i)])
                                          (second a))))]
                 {i (first best)}))
             (first a)))
       first-filter))

(count test)

(defn reduce-map
  [coll]
  (reduce-kv (fn [a b c]
               (if (> c (second a))
                 [b c]
                 a))
             [:start 0]
             coll))


