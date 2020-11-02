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
            (reduce + (map #(if (= %2 %1) 1 0) a b)))
          (reduce-map [coll]
            (reduce-kv (fn [a b c] (if (> c (second a)) [b c] a)) [:start 0] coll))
          (evaluate [word coll ]
            (apply hash-map (mapcat (fn [x] [x (fitness x word)]) coll)))
          (normalize [coll]
            (apply assoc {} coll))
          (categorize [group]
            (let [incompletes (first group)
                  completes (second group)]
              (map (fn [i-word]
                     (let [best (reduce-map (evaluate i-word completes))]
                       [i-word (first best)]))
                   incompletes)))]
    (let [[first-part second-part] (map normalize
                                        [(mapcat flatten (:ready first-filter))
                                         (->> (mapcat categorize (:not-ready first-filter))
                                              (flatten))])]
      (conj first-part second-part))))

(defn write-file []
  (with-open [wrtr (clojure.java.io/writer (io/resource "clojure_version/aufgabe1sample.txt") :append true)]
    (.write wrtr "hello")))

(write-file)
second-filter

(defn foo [coll]
  (if (not= coll '())
    (cons (second-filter (first coll)) (foo (rest coll)))
    nil))
(foo (:incompletes start))

(map (fn [w]
       (let [accent (re-find #"[,;:.]" w)]
         (update second-filter (clojure.string/replace w #"[,;:.]" "") #(str % accent))))
     (:accents start))



