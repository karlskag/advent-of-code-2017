(ns adventofcode.day4
  (:gen-class)
  (:require [clojure.test :refer [is]]
            [clojure.string :as str]))

(defn get-lines
  []
  (str/split-lines (slurp "resources/input-4.txt")))

(defn no-copies
  [split-phrase]
  (= (count split-phrase) (count (distinct split-phrase))))

(defn no-copies-or-palindromes
  [split-phrase]
  (and (= (count split-phrase) (count (distinct split-phrase)))
       (= (count split-phrase) (count (distinct (map (fn [word]
                                                       (sort word)) split-phrase))))))

(defn get-number-of-valid-pp
  {:test (fn []
           (is (= (get-number-of-valid-pp no-copies ["aaa bbb aa hkl"
                                                     "abs hkl abs"]) 1))
           (is (= (get-number-of-valid-pp no-copies-or-palindromes ["abcde fghij"
                                                                   "abcde xyz ecdab"
                                                                   "iiii oiii ooii oooi oooo"
                                                                   "oiii ioii iioi iiio"]) 2)))}
  [valid-condition phrases]
  (reduce (fn [number-of-valid phrase]
            (let [split-phrase (str/split phrase #"\s")]
              (if (valid-condition split-phrase)
                (inc number-of-valid)
                number-of-valid))) 0 phrases))

(defn advent-solution-4
  []
  (->> (get-lines)
       (get-number-of-valid-pp no-copies)
       (println "Solution to problem a: "))
  (->> (get-lines)
       (get-number-of-valid-pp no-copies-or-palindromes)
       (println "Solution to problem b: ")))

(advent-solution-4)


