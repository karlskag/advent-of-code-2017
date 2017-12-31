(ns adventofcode.day5
  (:gen-class)
  (:require [clojure.test :refer [is]]
            [clojure.string :as str]))

(defn get-maze
  []
  (vec (map read-string (str/split-lines (slurp "resources/input-5.txt")))))

(defn jump-in-maze
  [maze index update-action]
  (let [steps (nth maze index)
        new-maze (update-in maze [index] (update-action steps))]
    {:maze new-maze :current-index (+ steps index)}))

(defn advent-solution-5
  {:test (fn []
           (is (= (advent-solution-5 [0 3 0 1 -3] (fn [_] inc)) 5))
           (is (= (advent-solution-5 [0 3 0 1 -3] (fn [offset] (if (>= offset 3) dec inc))) 10)))}
  [maze update-action]
  (loop [new-maze maze current-index 0 counter 0]
    (if (or (>= current-index (count maze)) (< current-index 0))
      counter
      (let [new-maze-data (jump-in-maze new-maze current-index update-action)]
        (recur (:maze new-maze-data) (:current-index new-maze-data) (inc counter))))))

(print "Solution to problem a: ")
(-> (get-maze)
    (advent-solution-5 (fn [_] inc)))
(print "Solution to problem b: ")
(-> (get-maze)
    (advent-solution-5 (fn [offset] (if (>= offset 3) dec inc))))



