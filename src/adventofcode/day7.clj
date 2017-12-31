(ns adventofcode.day7
  (:gen-class)
  (:require [clojure.test :refer [is]]
            [clojure.string :as str]))

(defn read-data
  []
  (str/split-lines (slurp "resources/input-7.txt")))

(defn string->node
  {:test (fn []
           (is (= (string->node "node (70) -> c1, c2, c3") {:node {:weight 70 :children ["c1", "c2", "c3"] :name "node"}})))}
  [node-string]
  (let [[node-info child-info] (str/split node-string #"->")]
    {(keyword (first (str/split node-info #"\s")))
     {:weight   (first (read-string (second (str/split node-info #"\s"))))
      :children (if child-info (vec (map str/trim (str/split child-info #","))) [])
      :name     (first (str/split node-info #"\s"))}}))

(defn find-parent-node
  {:test (fn []
           (is (= (find-parent-node {:a {:weight 74, :children ["b"]}
                                     :b {:weight 32, :children ["c"]}
                                     :c {:weight 58, :children []}}) "a")))}
  [dep-tree]
  (let [child-node-names (reduce (fn [children node] (vec (flatten (conj children (:children (second node)))))) [] dep-tree)
        all-node-names (vec (map name (keys dep-tree)))]
    (first (filter (fn [node]
                     (neg? (.indexOf child-node-names node))) all-node-names))))

(defn create-dep-tree
  [data]
  (->> data
       (map string->node)
       (reduce (fn [coll entry]
                 (assoc coll (key (first entry)) (second (first entry)))) {}))) ;from collection of map-entries to map


(defn calc-weight-from-node
  {:test (fn []
           (is (= (calc-weight-from-node {:weight 74, :children ["b"]} {:a {:weight 74, :children ["b"]}
                                                                        :b {:weight 32, :children ["c"]}
                                                                        :c {:weight 58, :children []}}) 164)))}
  [root tree]
  (loop [nodes [root] sum-weight 0]
    (if (empty? nodes)
      sum-weight
      (recur (map (fn [key-string] ((keyword key-string) tree)) (reduce (fn [children node] (vec (flatten (conj children (:children node))))) [] nodes)) ;get all child nodes from tree-data
             (reduce (fn [s n] (+ s (:weight n))) sum-weight nodes)))))

(defn get-key-for-value
  [map value]
  (first (keep #(when (= (val %) value)
                  (key %)) map)))

(defn get-balancing-function
  {:test (fn []
           (is (= ((get-balancing-function {:bjks 1500
                                            :apkd 700
                                            :asdd 700}) 2000) 1200)))}
  [weight-map]
  (let [weights-to-freq (frequencies (vals weight-map))
        unbalanced-weight (get-key-for-value weights-to-freq (apply min (vals weights-to-freq)))
        balanced-weight (get-key-for-value weights-to-freq (apply max (vals weights-to-freq)))]
    (if (> unbalanced-weight balanced-weight) (fn [v] (- v (- unbalanced-weight balanced-weight)))
                                              (fn [v] (+ v (- balanced-weight unbalanced-weight))))))

(defn calc-weight-for-subtrees
  [child-node-names tree]
  (let [nodes (map (fn [key-string] ((keyword key-string) tree)) child-node-names)]
    (reduce (fn [coll node] (assoc coll (:name node) (calc-weight-from-node node tree))) {} (vec nodes))))

(defn get-weight-if-balanced
  [tree]
  (let [parent-node-name (find-parent-node (create-dep-tree (read-data)))
        balance (get-balancing-function (calc-weight-for-subtrees (:children ((keyword (find-parent-node tree)) tree)) tree))]
    (loop [parent-node ((keyword parent-node-name) tree)]
      (let [sub-tower-weights (calc-weight-for-subtrees (:children parent-node) tree)
            freq-map (frequencies (vals sub-tower-weights))]
        (if (= (count (distinct (vals sub-tower-weights))) 1)
          (balance (:weight parent-node))
          (recur ((keyword (get-key-for-value sub-tower-weights (get-key-for-value freq-map (apply min (vals freq-map))))) tree)))))))

(println "Solution to problem 7a:" (find-parent-node (create-dep-tree (read-data))))

(println "Solution to problem 7b:" (get-weight-if-balanced (create-dep-tree (read-data))))

