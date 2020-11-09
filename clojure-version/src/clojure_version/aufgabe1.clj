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

(def second-filter
  (letfn [(fitness [a b]
            (let [v (reduce + (map #(if (and (not= %1 %2)
                                             (not= %2 (first "_"))
                                             (not= %1 (first "_")))
                                      -1
                                      0)
                                   a b))]
              (if (neg? v) 0 1)))
          (reduce-map [coll]
            (reduce-kv (fn [a b c] (if (> c (second a)) [b c] a)) [:start 0] coll))
          (normalize [coll]
            (apply assoc {} coll))
          (path-finder [group]
            (let [incompletes (first group)
                  completes (second group)
                  karte (map (fn [x]
                               (map (fn [y]
                                      (fitness x y))
                                    completes))
                             incompletes)]
              karte))]
    (comment (let [[first-part second-part] (map normalize
                                          [(mapcat flatten (:ready first-filter))
                                           (->> (mapcat categorize (:not-ready first-filter))
                                                (flatten))])]
               (conj first-part second-part)))
    ))

(defn grid [a] (map (fn [_] (repeat 3 (rand-int 2))) (range a)))

(defn seek-places
  [grid]
  (letfn [(paolo [x]
            (comp
             (map-indexed (fn [i e] [i (nth e x)]))
             (filter #(pos? (second %1)))
             (map first)))]
    (comment (map #(transduce (paolo %1) conj [] grid) (range (count grid))))
    (transduce (paolo 0) conj [] grid)))

(seek-places grid)

(defn paolo [x]
  (comp
   (map-indexed (fn [i e] [i (nth e x)]))
   (filter #(pos? (second %1)))
   (map first)))

(transduce  (paolo 0) conj [] [[0 1 0] [0 1 0] [1 0 1]])
(transduce  (paolo 1) conj [] [[0 1 0] [0 1 0] [1 0 1]])
(transduce  (paolo 2) conj [] [[0 1 0] [0 1 0] [1 0 1]])

(map first (filter #(pos? (second %1)) (map-indexed (fn [i e] [i (nth e 0)]) [[0 1 0] [0 0 0] [1 0 1]])))

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

(defn combinatorics
  [f
   coll]
  (let [i (str (count coll) (first coll))]
    (if (empty? coll)
      f
      (map (fn [i] (foo (conj f i) (rest coll))) (first coll)))))

(defn accents [key-coll]
  (if (not= key-coll '())
    (update
     (zeta (rest key-coll))
     (clojure.string/replace (first key-coll) #"[,;:.]" "")
     #(str %1 (re-find #"[,;:.]" (first key-coll))))
    second-filter))

(defn order [coll]
  (if (not= coll '())
    (cons (second-filter (first coll)) (foo (rest coll)))
    nil))

(order (:incompletes start))
(count second-filter)
start
first-filter


(defn write-file []
  (with-open [wrtr (clojure.java.io/writer (io/resource "clojure_version/aufgabe1sample.txt") :append true)]
    (.write wrtr "hello")))

(write-file)
second-filter


