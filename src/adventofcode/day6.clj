(ns adventofcode.day6
  (:gen-class)
  (:require [clojure.test :refer [is]]
            [clojure.string :as str]))

(def input-5 "2\t8\t8\t5\t4\t2\t3\t1\t5\t5\t1\t2\t15\t13\t5\t14")

(defn first-loop
  [key map counter]
  (if (key map) counter nil))

(defn second-loop
  [key map counter]
  (let [sec-occ (second (key map))]
  (if sec-occ
    (- counter sec-occ)
    nil)))

(defn find-number-of-cycles
  "Implementation only supports first and second occurrence of combination"
  {:test (fn []
           (is (= (find-number-of-cycles first-loop [0 2 7 0]) 5))
           (is (= (find-number-of-cycles first-loop [2 8 8 5 4 2 3 1 5 5 1 2 15 13 5 14]) 3156)) ; correct result problem a
           (is (= (find-number-of-cycles second-loop [0 2 7 0]) 4)))}
  [show-result-if-calculated initial-allocation]
  (loop [combination initial-allocation found-combinations {} counter 0]
    (let [max-value (apply max combination)
          start-index (inc (.indexOf combination max-value))
          working-allocation (update-in combination [(.indexOf combination max-value)] (fn [x] (- x x)))
          next-allocation (loop [working-combination working-allocation counter start-index]
                            (if (= counter (+ start-index max-value))
                              working-combination
                              (recur (update-in working-combination [(mod counter (count working-allocation))] inc) (inc counter))))]
      (or (show-result-if-calculated (keyword (str/join combination)) found-combinations counter)
          (recur (vec next-allocation)
                 (assoc found-combinations (keyword (str/join combination)) (conj (or ((keyword (str/join combination)) found-combinations) []) counter))
                 (inc counter))))))

(->> (vec (map read-string (str/split input-5 #"\t")))
     (find-number-of-cycles first-loop)
     (println "Solution to problem 6a:"))
(->> (vec (map read-string (str/split input-5 #"\t")))
     (find-number-of-cycles second-loop)    ;81ms
     (println "Solution to problem 6b:"))