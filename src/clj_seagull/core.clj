(ns clj-seagull.core
  (:require [loom.graph :as loom]
            [clojure.string :as str]
            [clojure.core.reducers :as r]
  (:gen-class)))

(defn cooccurrences
  "Get coccurrences from tokenized text.
  
  Though intended for words, `xs` can be any list. Because this function is
  designed to be input into a bi-directional graph, no effort is made to
  maintain symmetry.

  Example: 
  (cooccurrences [\"fly\" \"my\" \"pretties\"])
  >>> {\"fly\" {\"my\" 1, \"pretties\" 1},
       \"my\" {\"pretties\" 1}, \"pretties\" {}}   "
  [xs]
  (loop [acc {} 
         xs xs]
    (if (empty? xs)
      acc
      (recur 
        (update acc 
               (first xs) 
               (fn [m] (merge-with + m (frequencies (take 5 (rest xs))))))
        (rest xs)))))
       
(defn txt-to-lex-graph
  "Turns a string of text into a weighted graph of tokens.
  
  The sum of the coccurrence counts will be the weight of a connection, so
  the more often a word occurs next to another, the strong the tie between
  the two.
  
  The return object will be a `loom.graph/weighted-graph`."
  [txt]
  (loom/weighted-graph
  (reduce 
    (fn [acc nxt] 
      (merge-with (fn [x y] (merge-with + x y)) acc (cooccurrences nxt)))
    {}
    ;;TODO: replace with real tokenization functions
    (map (fn [s] (filter (fn [x] (not= "" x)) (str/split s #"\s+")))
      (str/split txt #"[.?!]")))))

(defn weighted-next-steps
  "Returns a weighted sequence of possible next steps.
  
  Finds all possible nodes connecting to node `n` on graph `g` and 
  returns a sequence of these nodes, with each node occurring a number
  of times equal to the weight of the edge between the nodes.
  
  For example, if node 'a' is connected to 'b' with weight 2 and 'c' 
  with weight 1, then the resulting sequence would be: ['b' 'b' 'c']"
  [g n]
  (reduce (fn [acc [n1 n2]] (into acc (repeat (loom.graph/weight g n1 n2) n2))) [] (loom.graph/out-edges g n)))
  
(defn random-walk
  "Return a randomly walked path on graph `g` from a `start` node.
  
  Starting from the node `start` on graph `g` and continuing until `steps`
  number of steps have been taken, `random-walk` will continue along the
  graph by randomly selecting a neighboring node. The probability of
  a neighboring node being selected is equal to the ratio of the  weight 
  of the edge between the current node and that node and the sum of the
  edge weights for the current node. For example, if we are currently on 
  node 'a', which has edges between 'b' (weight 3) and c (weight 1), the
  probability fo moving to b is 3/(3+1).

  The returned object is a `steps` length sequence containing the path
  of the random walk."
  [g start steps]
  (loop [pth [start]]
    (if (= (count pth) steps) pth
      (recur (conj pth (rand-nth (weighted-next-steps g (first pth))))))))

(defn seeded-walk-freqs
  "Get the frequency with which words appear in a number of random walks.
  
  Performs a number of random `walks` along a given weighted-graph `g`.
  The random walks may occur from one of several starting points,
  or `seeds`. The total number of walks will be equal to 
  `walks` times (count `seeds`). The total number of steps taken will 
  equal `steps` times that number.

  Each walk is taken independent of previous walks.

  The frequency at which nodes were visited on all the random walks is returned."
  [g steps walks seeds]
  (r/fold 
    (fn ([]{}) 
        ([acc nxt] (merge-with + acc (frequencies (random-walk g nxt steps)))))
    (reduce (fn [acc nxt] (into acc (repeat walks nxt))) [] seeds)))
    
(defn multiple-seeded-walks
  "Perform seeded random walks on multiple sets of seed."
  [g steps walks & seeds]
  (map (fn [x] (seeded-walk-freqs g steps walks x)) seeds))

(defn generate-lexicon
  "Generate a contrastive lexicon by juxtiposing multiple seeded random walks"
  [fs]
  (let [c (apply (fn [a b] (merge-with + a b)) fs)]
  (reduce (fn [acc x] (conj acc (merge-with / x c))) [] fs)
))

(defn -main
  "Empty main function."
  [& args]
  (println "Hello, World!"))
