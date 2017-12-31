(ns adventofcode.day3
  (:gen-class)
  (:require [clojure.test :refer [is]]
            [clojure.string :as str])
  (:import [java.lang Math]))

(def input-3 325489)

(defn get-min
  {:test (fn []
           (is (= (get-min [1 0] [[0 0] [-1 0]]) -1))
           (is (= (get-min [0 1] [[0 0] [-1 0] [0 -1] [0 -2]]) -2)))}
  [direction matrix]
  (if (or (= direction [1 0]) (= direction [-1 0]))
    (apply min (map first matrix))
    (apply min (map second matrix))))

(defn get-max
  {:test (fn []
           (is (= (get-max [1 0] [[0 0] [-1 0]]) 0))
           (is (= (get-max [0 1] [[0 2] [-1 0] [0 -1] [0 -2]]) 2)))}
  [direction matrix]
  (if (or (= direction [1 0]) (= direction [-1 0]))
    (apply max (map first matrix))
    (apply max (map second matrix))))

(defn farthest-in-direction?
  {:test (fn []
           (is (= (farthest-in-direction? [0 1] second get-max [[0 0] [1 0] [1 1]]) true)))}
  [direction axis minmax coordinates]
  (let [last-cell (last coordinates)]
    (and (= (minmax direction coordinates) (axis last-cell))
         (= (count (filter (fn [coords]
                             (= (axis coords) (axis last-cell))) coordinates)) 1))))

(defn calculate-sum
  {:test (fn []
           (is (= (calculate-sum [0 0] {:11 4 :01 5 :-1-1 4 :0-1 4}) 17))
           (is (= (calculate-sum [0 1] {:00 1 :10 1 :11 2}) 4)))}
  [[x y] cell-sums]
  (let [neighbours [[(inc x) y]                             ;right
                    [(dec x) y]                             ;left
                    [x (inc y)]                             ;above
                    [x (dec y)]                             ;below
                    [(inc x) (inc y)]                       ;right-above
                    [(dec x) (inc y)]                       ;left-above
                    [(inc x) (dec y)]                       ;right-below
                    [(dec x) (dec y)]]]                     ;left-below
    (reduce (fn [sum neighbour]
              (let [sum-or-zero (or ((keyword (str/join neighbour)) cell-sums) 0)]
                (+ sum sum-or-zero))) 0 neighbours)))

(defn create-spiral-data
  [last-cell direction corners cells-calculated cell-sums]
  {:last-cell        last-cell
   :direction        direction
   :corners          corners
   :cells-calculated cells-calculated
   :cell-sums        cell-sums})

(defn update-spiral
  [working-data axis delta minmax [turn keep-going]]
  (if (farthest-in-direction? (:direction working-data) axis minmax (conj (:corners working-data) (:last-cell working-data)))
    (let [new-cell turn]
      (create-spiral-data new-cell delta (vec (take-last 4 (conj (:corners working-data) (:last-cell working-data))))
                          (inc (:cells-calculated working-data)) (assoc (:cell-sums working-data)
                                                                   (keyword (str/join new-cell)) (calculate-sum new-cell (:cell-sums working-data)))))
    (let [new-cell keep-going]
      (create-spiral-data new-cell (:direction working-data) (:corners working-data)
                          (inc (:cells-calculated working-data)) (assoc (:cell-sums working-data)
                                                                   (keyword (str/join new-cell)) (calculate-sum new-cell (:cell-sums working-data)))))))

(defn get-next-matrix
  {:test (fn []
           (is (= (get (get-next-matrix (create-spiral-data [1 0] [1 0] [[0 0]] 2 {})) :last-cell) [1 1]))
           (is (= (get (get-next-matrix (create-spiral-data [1 1] [0 1] [[0 0] [1 0]] 3 {})) :last-cell) [0 1]))
           (is (= (get (get-next-matrix (create-spiral-data [-1 1] [-1 0] [[0 0] [1 0] [1 1]] 5 {})) :last-cell) [-1 0]))
           (is (= (get (get-next-matrix (create-spiral-data [0 -1] [1 0] [[1 0] [1 1] [-1 1] [-1 -1]] 8 {})) :last-cell) [1 -1])))}
  [data]
  (let [working-data {:last-cell        (:last-cell data)
                      :direction        (:direction data)
                      :corners          (:corners data)
                      :cells-calculated (:cells-calculated data)
                      :cell-sums        (:cell-sums data)}]
    (if (= (:cells-calculated working-data) 0)
      (create-spiral-data [0 0] [1 0] [[0 0]] 1 {:00 1})
      (cond
        (= [1 0] (:direction working-data)) (update-spiral working-data first [0 1] get-max
                                                           [[(first (:last-cell working-data)) (inc (second (:last-cell working-data)))]
                                                            [(inc (first (:last-cell working-data))) (second (:last-cell working-data))]])
        (= [-1 0] (:direction working-data)) (update-spiral working-data first [0 -1] get-min
                                                            [[(first (:last-cell working-data)) (dec (second (:last-cell working-data)))]
                                                             [(dec (first (:last-cell working-data))) (second (:last-cell working-data))]])
        (= [0 1] (:direction working-data)) (update-spiral working-data second [-1 0] get-max
                                                           [[(dec (first (:last-cell working-data))) (second (:last-cell working-data))]
                                                            [(first (:last-cell working-data)) (inc (second (:last-cell working-data)))]])
        (= [0 -1] (:direction working-data)) (update-spiral working-data second [1 0] get-min
                                                            [[(inc (first (:last-cell working-data))) (second (:last-cell working-data))]
                                                             [(first (:last-cell working-data)) (dec (second (:last-cell working-data)))]])))))

(defn cell-number->spiral-data
  {:test (fn []
           (is (= (get (cell-number->spiral-data 0) :last-cell) []))
           (is (= (get (cell-number->spiral-data 1) :last-cell) [0 0]))
           (is (= (get (cell-number->spiral-data 2) :last-cell) [1 0]))
           (is (= (get (cell-number->spiral-data 3) :last-cell) [1 1]))
           (is (= (get (cell-number->spiral-data 6) :last-cell) [-1 0])))}
  [cell-number]
  (loop [spiral-data (create-spiral-data [] [0 0] [] 0 {})]
    (if (= (:cells-calculated spiral-data) cell-number)
      spiral-data
      (recur (assoc (get-next-matrix spiral-data) :cell-sums {}))))) ;solution a doesn't need cell-sums

(defn calculate-distance-first-last
  [spiral-data]
  (+ (Math/abs (- 0 (first (:last-cell spiral-data)))) (Math/abs (- 0 (second (:last-cell spiral-data))))))

(defn first-cell-value-above
  {:test (fn []
           (is (= (first-cell-value-above 8) 10))
           (is (= (first-cell-value-above 350) 351)))}
  [test-value]
  (loop [spiral-data (create-spiral-data [] [0 0] [] 0 {})]
    (if (> (or ((keyword (str/join (:last-cell spiral-data))) (:cell-sums spiral-data)) 0) test-value)
      ((keyword (str/join (:last-cell spiral-data))) (:cell-sums spiral-data))
      (recur (get-next-matrix spiral-data)))))

(defn advent-solution-3
  "Solution to problem #3 a in advent of code 2018"
  {:test (fn []
           (is (= (advent-solution-3 1) 0))
           (is (= (advent-solution-3 12) 3))
           (is (= (advent-solution-3 23) 2))
           (is (= (advent-solution-3 1024) 31)))}
  [input]
  (->> (cell-number->spiral-data input)
       (calculate-distance-first-last)))

(println "Solution a:" (advent-solution-3 input-3))
(println "Solution b:" (first-cell-value-above input-3))
