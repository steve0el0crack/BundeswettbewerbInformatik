(ns clojure-version.core
  (:require [clojure.java.io :as io]))

(defn start []
  (let [file (with-open [rdr (clojure.java.io/reader (io/resource "clojure_version/example.txt"))]
               (reduce conj [] (line-seq rdr)))]
    (map #(clojure.string/split %1 #" ") file)))

(defn value
  [text]
  (let [words (second (start))]
    ))

(first (start))

(def counted (map (fn [f] (apply hash-map (mapcat (fn [x] [x (count x)]) (f (start))))) [first second]))  

(def freq (map (fn [y] (frequencies (vals (y counted)))) [first second]))

(def unique-set (apply clojure.set/union (map (fn [b] (set (vals (b counted)))) [first second])))

(defstruct (into [] unique-set))

(for [coll counted]
  (for [elem coll]
    ))
