(ns clojure-version.core 
  (:require [clojure.java.io :as io]))

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

;; Will contain two dictionaries (Hashmaps) in a list: The first for the incomplete words and the second for the complete ones.
;; This will be the structure: [{"___e" : 4, "__d" : 3, ...} {"eine" : 4, "und" : 3, ...}]

(def search-map-by-val
  (comp
   (filter #(= (second %1) 1))
   (map first)))

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
          (solved [] (if (solvable?) (set (transduce search-map-by-val conj [] (first (freq)))) #{}))
          (unique-set [] (set (keys (first (freq)))))
          (wrap [n] ;; wrap can be implemented only changing a parameter in search-map-by-val
           (map (fn [f]
                   (map first
                        (filter #(= (last %1) n)
                                (f (label)))))
                [first second]))]
    (let [[not-ready ready] (map (fn [a]
                                   (apply hash-map
                                          (mapcat #(wrap %1) a)))
                                 [(clojure.set/difference (unique-set) (solved))
                                  (solved)])]
      {:ready ready ;; ... of the form {("a_s")(aus)} there is only one combination.
       :not-ready not-ready})))

(defn fitness
  [a b]
  (reduce + (map #(if (= %2 %1) 1 0) a b)))

(defn reduce-map
  [coll]
  (reduce-kv (fn [a b c]
               (if (> c (second a))
                 [b c]
                 a))
             [:start 0]
             coll))

(def second-filter
  (apply assoc {} (flatten (mapcat (fn [a]
                                     (map (fn [i]
                                            (let [best (reduce-map
                                                        (apply hash-map
                                                               (mapcat (fn [x]
                                                                         [x (fitness x i)])
                                                                       (second a))))]
                                              [i (first best)]))
                                          (first a)))
                                   (:not-ready first-filter)))))

(count (conj second-filter (apply assoc {} (mapcat flatten (:ready first-filter)))))



