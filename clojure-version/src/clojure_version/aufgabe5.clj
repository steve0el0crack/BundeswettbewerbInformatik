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
        input (with-open [rdr (clojure.java.io/reader (io/resource "clojure_version/aufgabe5sample3.txt"))]
                (reduce conj [] (line-seq rdr)))]
    (transduce hey conj [] input)))

(def data (map-indexed (fn [i v] [i v]) (rest input)))

;; {:per-column [[...] [...] [...]] :f #{...} :s #{...} :l #{...}}

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
  ;; this maps each UNIQUE VARIABLE with its indexes of ocurrence on the each COLUMN
  [data]
 (map (fn [f k]
        (apply conj (map (fn [v] {v (->> (f (:per-column (description data)))
                                         (filter #(= (second %1) v))
                                         (map first))})
                         (k (description data)))))
       [first second last] [:f :s :l])) ;; I can wrap this within a GREATER FUNCTION....

;; v's are the unique numbers (they are all different) and the coll's are the positions at which they appear at :x column.
;; [{v1 [...], v2 [...], v3 [...]} -> :f irst column
;;  {va [...], vb [...]}           -> :s econd column
;;  {vx [...] ... }]               -> :l ast column

;; *************** I THINK I CAN JUST GET RIDD OF THIS PIECE OF CODE AND EVERYTHING WILL STILL BE FINE **********
;; Is there any variable with just one possibility? It means if there is a number on any column that appears just once.
(comment (def fixed-vars (count (map (fn [h] (filter #(= 1 (count (second %1))) h)) (structure data))))) ;; needs more code to get to the pair (key value)
;; ********************** ******************

;; This function is going to pick ONE KID/COORDINATE for each GIFT.
;; In an specified COLUMN. For example
;; [[x y z] [a b c]] -> [x a]
;; means that there are two variables/gifts which MUST BE given as FIRST WISH. There are possible coordinates [x y z] for the first variable (gift) (kids x y z DO WANT this gift as their first wish) and there are possible coordinates [a b c] for the second variable.
;; The problem is HOW TO CHOOSE ONE KID/COORDINATE FOR EACH VARIABLE (in this case 2 -one out of each [coll]-) SUCH THAT OTHER GIFTS STILL DO HAVE KIDS TO BE GIVEN TO!
(defn find-configuration
  [parameter  ;; changes if necessary...
   pool       ;; the group of possible coordinates [[a b c] [d e f ...] ...] There is one group of indexes for each variable.
   cond1      ;; If I choose indexes out of pool such that at the end I covered all the possibilities of another variable, 
   cond2]     ;; then it is a good configuration...
  (let [vars (repeatedly (count pool) logic/lvar)]  ;; I need ONE OUT OF EACH COLL of possible indexes... 
    (logic/run 1 [q]

      (rec-membero vars pool)

      (macro/symbol-macrolet [S (map #(remove (fn [_] (= _ nil)) %1) (Superset [] vars))]
                             (fd/<= (count (remove #(not-membero S %1) cond1)) parameter)  ;; this is for continuing trying with other values
                             (fd/<= (count (remove #(not-membero S %1) cond2)) parameter)) ;; " "
      
      (logic/== q vars))))

;; this is going to search for the coordinates (indexes) which should be marked in order to NOT BLOCK other unique variables...
;; When finding the first configuration, I must take care of not blocking the NEXT OTHER TWO COLUMNS.
;; But for further it is not going to be so...
(defn zeta
  [n data a b]
  (letfn [(phi [f]
            (map second (f (structure data))))]  ;; think I can use APPLY here...
    (if (empty? (find-configuration n
                                    (phi a)
                                    (phi b)
                                    (phi last)))
      (zeta (+ n 1))
      (find-configuration n
                          (phi a)
                          (phi b)
                          (phi last)))))


;; Till now, we have find the following:
;; * fixed-vars -> Unique variables that must be marked on the first COLUMN, since they are nowhere else (the reasoning behind this procedure is that if they are not marked at THAT COLUMN and at THAT INDEX, then there are two possibilities: It is going to be marked on the one of the 2 next columns, which would give us at the end a WORSE RATING for the selection. The second possibility is that they are not going to be marked at all! That would mean that at the end of the distribution/assignment of gifts; one kid will receive this gift without even having wished for it... it would be completely random)
;; * hello -> Focusing on those vars which could be positioned at many different indexes, the question is which of these configurations to choose? With the help of a logic engine; one can find the configuration that leaves THE MOST/BETTER ROOM for other variables. The reasoning behind this is that sometimes choosing one of 2 possible coordinates for a determined variable WILL MAKE the difference when choosing the following.

;; In other words, till now we should have the following: "On the first COLUMN the numbers/variables/gifts [...] should be marked at the index's [...]". That means... the first row is complete! And the rest is just repeating the same, taking into account other "parameters"...

;; ***************************** INTERMEDIATE FUNCTIONS **********************

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

(defn substract
  ;; remove elements at index [indexs] in an indexed-collection [[0 ...] [1 ...]] <- [[...] [...]]
  [coll indexs]
  (remove (fn [coll] (some #(= (first coll) %1) indexs)) coll))

;; ******************************* CONTINUING *******************************

;; This means that ON THE FIRST ROW I mus cross the numbers (:f description) on the indexes/ for the kids (first (zeta 0))
(def first-column-pairing {(:f (description data)) (first (zeta 0 data first second))})

;; And this are the rows/kids left for the next pairing
(def after-first-column-pairing (substract data (first (zeta 0 data first second))))

;; Now I must do exactly the same... there are some unique variables on the second column.
;; And I must cross them so that the other unique variables on the third column can still be crossable.
;; Or at least I must find the MARKS that would be necessary to minimize the amount of VARIABLES LEFT OUT OF THE GAME!

;; Then I describe the resting rows...
(description after-first-column-pairing) ;; (description data) not that these two are different since (zeta 0 data) indexes are not going to be taken into account anymore... THEY WERE ALREADY CROSSED! (zeta 0 after-first-column-pairing first second) This would give me another set of INDEXES to be crossed on the FIRST column.
(structure after-first-column-pairing)

;; And when finding the configuration (the numbers and indexes which must be crossed),
;; the first COLUMN will not be taken into account, but instead the second and the third one.
;; [The reasoning behing this is -like the first time- to pick one INDEX for each VARIABLE such that those
;; variables gifts that MUST BE GIVEN AS THIRD WISH could still be... ]

(def second-column-pairing {(:s (description after-first-column-pairing)) (first (zeta 0 after-first-column-pairing second last))})

(def after-second-column-pairing (substract after-first-column-pairing (first (zeta 0 after-first-column-pairing second last))))

;; Now I have only ONE COLUMN LEFT... I MUST DO quasi THE SAME...
;; At the end I can choose one random out of EACH GROUP... I do not need the help of the logic engine!

(def third-column-pairing
  (let [final (map (fn [coll] [(first coll)               ;; GIFT
                               (rand-nth (second coll))]) ;; KID's position
                   (last (structure after-second-column-pairing)))]
    {(set (map first final))
     (map second final)}))

;; At the end there shall be some kids (indexes) who were not assigned anything...there there is also some gifts "free"
;; Let's recollect'em!

(def free-spots
 (let [r (set (range (first (first input))))]
   {:kids (set/difference r
                          (set/union (first (first first-column-pairing))
                                     (first (first second-column-pairing))
                                     (first (first third-column-pairing))))
    :gifts (set/difference r
                           (set/union (second (first first-column-pairing))
                                      (second (first second-column-pairing))
                                      (second (first third-column-pairing))))}))





