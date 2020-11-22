(ns clojure-version.aufgabe5
  (:require [clojure.java.io :as io]
            [clojure.core.logic :as logic]
            [clojure.core.logic.fd :as fd]
            [clojure-version.aufgabe1 :as aufgabe1]
            [clojure.tools.macro :as macro]
            [clojure.set :as set]))

;; AFGABE 5: Wichteln

(logic/defne rec-membero
  [vars nested-coll]
  ([_ []])
  ([[x . xr] [S . rS]]
   (logic/membero x S)
   (rec-membero xr rS)))

(logic/defne rec-notmembero
  [l coll]
  ;; ANY member of coll is a member of l... sort of negation of membero |
  ([_ []])
  ([_ [?x . ?r]]   
   (not-membero ?x l)
   (rec-notmembero l ?r)))

(logic/defne ==coll
  [coll1 coll2]
  ([_ _]
   (logic/permuteo coll1 coll2)))

(logic/defne not-membero [x l]
  ([_ []])
  ([_ [?y . ?r]]
   (logic/!= x ?y)
   (not-membero x ?r)))

(logic/defne membero-coll
  [coll1 nested-coll]
  ([_ _]
   (logic/fresh [a]
     (logic/membero a nested-coll)
     (logic/permuteo a coll1))))

(defn Superset
  [c set]
  (let [i (gensym)]
    (if (= (count set) 1)
      (map (fn [a] (conj c a)) [nil (first set)])
      (mapcat (fn [i] (Superset (conj c i) (rest set))) [nil (first set)]))))

(defn substract
  ;; remove elements at index [indexs] in an indexed-collection [[0 ...] [1 ...]] <- [[...] [...]]
  [coll indexs]
  (remove (fn [coll] (some #(= (first coll) %1) indexs)) coll))

(def input
  (let [hey (comp
             (map #(clojure.string/split %1 #" "))
             (map (fn [_] (remove #(= %1 "") _)))
             (map #(map read-string %1)))
        input (with-open [rdr (clojure.java.io/reader (io/resource "clojure_version/aufgabe5sample.txt"))]
                (reduce conj [] (line-seq rdr)))]
    (transduce hey conj [] input)))

(def data (map-indexed (fn [i v] [i v]) (rest input)))

(defn description
  [data]
  (let [per-column (map (fn [f]
                          (map (fn [v] [(first v) (f (second v))])
                               data))
                        [first second last])
        uniques (map (fn [v] (set (map second v))) per-column)
        first-wish (first uniques)
        second-wish (set/difference (second uniques) (first uniques))
        third-wish   (set/difference (last uniques) (set/union (second uniques) (first uniques)))]
    {:per-column per-column :f first-wish :s second-wish  :l third-wish}))

(defn structure
  [data]
 (map (fn [f k]
        (apply conj (map (fn [v] {v (->> (f (:per-column (description data)))
                                         (filter #(= (second %1) v))
                                         (map first))})
                         (k (description data)))))
       [first second last] [:f :s :l])) 

(defn find-configuration
  [parameter pool cond1 cond2]
  (if (empty? pool)
    '()
    (let [vars (repeatedly (count pool) logic/lvar) ;; THIS IS ACTUALLY GOING TO BOTTOM! 
          S (map #(remove (fn [_] (= _ nil)) %1) (Superset [] vars))]  
      (logic/run 1 [q]

        (rec-membero vars pool) ;; I think this recursive function goes to BOTTOM if pool is '() nil... yes definitely

        (macro/symbol-macrolet [a (count (filter (fn [c] (membero-coll c S)) cond1))
                                b (count (filter (fn [c] (membero-coll c S)) cond2))]
                               (fd/eq
                                (<= a parameter)
                                (<= b parameter))) 
        
        (logic/== q vars)))))

(defn zeta
  [n pool arg1 arg2]
  (let [answer (find-configuration n
                                   pool
                                   arg1
                                   arg2)]
    (if (empty? answer)
      (zeta (+ n 1)
            pool
            arg1
            arg2)
      answer)))


(def first-column-pairing {(:f (description data)) (first (zeta 0
                                                                (map second (first (structure data)))
                                                                (map second (second (structure data)))
                                                                (map second (last (structure data)))))})
(def after-first-column-pairing (substract data (first (zeta 0
                                                             (map second (first (structure data)))
                                                             (map second (second (structure data)))
                                                             (map second (last (structure data)))))))

(def second-column-pairing 
  (let [new-second-column (apply dissoc (second (structure after-first-column-pairing)) (first (keys first-column-pairing)))
        new-last-column (apply dissoc (last (structure after-first-column-pairing)) (first (keys first-column-pairing)))
        coords (first (find-configuration 10
                                          (map second new-second-column)
                                          (map second new-last-column)
                                          (map second new-last-column)))]
    {(set (keys new-second-column)) coords}))
(def after-second-column-pairing (substract after-first-column-pairing (first (vals second-column-pairing))))

(def third-column-pairing
  (let [final (map (fn [coll] [(first coll)               
                               (rand-nth (second coll))]) 
                   (apply dissoc (last (structure after-second-column-pairing)) (set/union
                                                                                 (first (keys second-column-pairing))
                                                                                 (first (keys first-column-pairing)))))]
    {(set (map first final))
     (map second final)}))

(def free-spots
 (let [r (set (range (first (first input))))]
   {(set/difference (set (range 1 (+ (first (first input)) 1)))
                          (set/union (first (first first-column-pairing))
                                     (first (first second-column-pairing))
                                     (first (first third-column-pairing))))
    (set/difference r
                    (set/union (second (first first-column-pairing))
                               (second (first second-column-pairing))
                               (second (first third-column-pairing))))}))

(defn true-test []
  (let [answer (map (fn [f]
                      (apply set/union (map (fn [c]
                                              (f (first c)))
                                            [first-column-pairing
                                             second-column-pairing
                                             third-column-pairing
                                             free-spots])))
                    [first second])]
    (= (count (first answer)) (count (second answer)))))



(defn answer-paolo []
  (let [output (if (true-test)
                 (str "\n The kids who receive their first wish are: " (into [] (first (vals first-column-pairing))) "\n"
                      ",those who receive their second wish are: " (into [] (first (vals second-column-pairing))) "\n"
                      ", and the ones receiving their third/last wish are " (into [] (first (vals third-column-pairing)))
                      (if (empty? (first (vals free-spots)))
                        ""
                        (str "\n And the kids " (into [] (first (vals free-spots))) " will receive one andom gift out of the following gifts " (into [] (first (keys free-spots))))))
                 "Something went wrong")]
    (with-open [wrtr (clojure.java.io/writer (io/resource "clojure_version/aufgabe5sample3.txt") :append true)]
      (.write wrtr output))
    output))

(answer-paolo)










