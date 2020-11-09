(ns clojure-version.core 
  (:require [clojure.java.io :as io]))

;; AUFGABE 1: Woerter aufraeumen 

(def start
  (let [file (with-open [rdr (clojure.java.io/reader (io/resource "clojure_version/aufgabe1sample.txt"))]
               (reduce conj [] (line-seq rdr))) 
        [readed accent] (loop [i 0
                               pool (-> (first file)
                                        (clojure.string/split #" ")) ;; 20 elements...
                               readed []
                               accent {}]
                          (if (= i (count (-> (first file)
                                              (clojure.string/split #" "))))
                            [readed accent pool]
                            (if (re-find #"[,;:.]" (first pool))
                              (recur (inc i)
                                     (rest pool)
                                     (conj readed (clojure.string/replace (first pool) #"[,;:.]" ""))
                                     (assoc accent (clojure.string/replace (first pool) #"[,;:.]" "") (first pool)))
                              (recur (inc i)
                                     (rest pool)
                                     (conj readed (first pool))
                                     accent))))]
    {:incompletes readed
     :completes (-> (second file)
                    (clojure.string/split #" "))
     :accents accent
     :raw (-> (first file)
              (clojure.string/split #" "))}))

(def search-map-by-val
  (comp
   (filter #(= (second %1) 1))
   (map first)))

(defn normalize
  [coll]
  (apply assoc {} coll))

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
      {:ready (normalize (mapcat flatten ready)) 
       :not-ready not-ready})))

(defn seek-places
  [grid]
  (letfn [(paolo []
            (comp
             (map-indexed (fn [i e] [i e]))
             (filter #(pos? (second %1)))
             (map first)))]
    (map #(transduce (paolo ) conj [] %1) grid)))

(defn combinatorics
  [coll]
  (letfn [(foo [f
                coll]
            (let [i (str (count coll) (first coll))]
              (if (empty? coll)
                [f]
                (mapcat (fn [i] (foo (conj f i) (rest coll))) (first coll)))))]
    (foo [] coll)))

(def second-filter
  (letfn [(connection? [a b]
            (let [v (reduce + (map #(if (and (not= %1 %2)
                                             (not= %2 (first "_"))
                                             (not= %1 (first "_")))
                                      -1
                                      0)
                                   a b))]
              (if (neg? v) 0 1)))
          (reduce-map [coll]
            (reduce-kv (fn [a b c] (if (> c (second a)) [b c] a)) [:start 0] coll))
          (find-path [group]
            (let [incompletes (first group)
                  completes (second group)
                  karte (map (fn [x]
                               (map (fn [y]
                                      (connection? x y))
                                    completes))
                             incompletes)
                  paths (->> karte
                             seek-places
                             combinatorics
                             (filter #(= (count (set %1)) (count %1))))]
              (map (fn [i c]
                     [i (nth completes c)])
                   incompletes (rand-nth paths))))]
    (let [worked (normalize (flatten (map find-path (:not-ready first-filter))))]
      (conj (:ready first-filter) worked))))

;; .......... EXPERIMENTATION Generalization using GENETIC ALGORITMS .............

(defn grid
  [a]
  (let [r (range a)]
    (for [x r] (for [y r] (rand-int 2)))))

(defn update
  [coll x]
  (remove #(= %1 x) coll))

(defn path-finder
  [grid]
  (loop [i 0
         candidates (seek-places grid)
         chosen (rand-nth (nth candidates i))
         next-candidates (map #(update %1 chosen) candidates)
         path []]
    (if (= i (count grid))
      path
      (if (empty? (nth next-candidates (+ i 1)))
        (recur i
               candidates
               (rand-nth (nth candidates i))
               (map #(update %1 (rand-nth (nth candidates i))) candidates)
               path)
        (recur (inc i)
               (map #(update %1 chosen) candidates)
               (rand-nth (map #(update %1 chosen) candidates))
               (conj chosen))))))

(path-finder test)

;; ...............................................................................

(defn order
  [scheme
   hash-map]
  (if (not= scheme '())
    (cons (hash-map (first scheme)) (order (rest scheme) hash-map))
    nil))

(defn answer []
  (let [smap (conj (normalize (mapcat #(let [x (re-find #"[,;:.]" (second %1))
                                             v (second-filter (first %1))]
                                         [v (str v x)])
                                      (:accents start)))
                   (:accents start))
        coll1 (normalize (replace smap (mapcat flatten second-filter)))
        ordered (order (:raw start) coll1)]
    (with-open [wrtr (clojure.java.io/writer (io/resource "clojure_version/aufgabe1sample.txt") :append true)]
      (.write wrtr (clojure.string/join " "  ordered)))))

(answer)



