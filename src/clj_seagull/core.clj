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
       \"my\" {\"pretties\" 1}, \"pretties\" {}}"
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
  ([txt]
  (let [s (fn [x] (str/split x #"[.?!]"))
        w (fn [x] (str/split x #"\s+"))]
  (txt-to-lex-graph txt {:sent s :word w})))
  ([txt tknzr]
  (let [sf (tknzr :sent) wf (tknzr :word)]
  (loom/weighted-graph
  (reduce
    (fn [acc nxt]
      (merge-with (fn [x y] (merge-with + x y))
                            acc (cooccurrences nxt)))
    {}
    (map
      (fn [s] (filter (fn [x] (not= "" x)) (wf s)))
      (sf txt)))))))

(defn weighted-next-steps
  "Returns a weighted sequence of possible next steps.

  Finds all possible nodes connecting to node `n` on graph `g` and
  returns a sequence of these nodes, with each node occurring a number
  of times equal to the weight of the edge between the nodes.

  For example, if node 'a' is connected to 'b' with weight 2 and 'c'
  with weight 1, then the resulting sequence would be: ['b' 'b' 'c']"
  [g n]
  (reduce (fn [acc [n1 n2]]
              (into acc (repeat (loom.graph/weight g n1 n2) n2)))
          []
          (loom.graph/out-edges g n)))

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

  Each walk is taken independent of previous walks. Walks occur in parallel.

  The frequency at which nodes were visited on all the random walks is returned."
  [g steps walks seeds]
  (let [xs (reduce (fn [acc nxt] (into acc (repeat walks nxt))) [] seeds)]
    (r/fold
      (fn ([] {}) ([coll1 coll2] (merge-with + coll1 coll2)))
      (fn [acc nxt] (merge-with + acc (frequencies (random-walk g nxt steps))))
      xs)))

(defn multiple-seeded-walks
  "Perform seeded random walks on multiple sets of seeds.

  This is identitical to calling `seeded-walk-freqs` with identical values for
  `steps` and `walks multiple times on multiple seed lists. Any number of seed
  sequences can be used. `seeds` are expected to be a sequence of nodes, just
  like in `seeded-walk-freq`.

  Returns the frequencies for the walks corresponding to each set of seeds.

  Example:

    (multiple-seeded-walks mygraph 25 1000 [\"first\" \"seeds\"]
                                           [\"second\" \"bunch\"])"
  [g steps walks & seeds]
  (map (fn [x] (seeded-walk-freqs g steps walks x)) seeds))

(defn generate-lexicon
  "Generate contrastive lexicons by juxtiposing multiple seeded random walks.

  Generally, takes any number of frequencies and returns new maps with each
  count replaced by the ratio of occurences in the original frequency and
  the total rate of occurrence.

  For example:

    (generate-lexicon [{\"this\" 1 \"is\" 2 \"a\" 3 \"test\" 4}
                       {\"no\" 1 \"this\" 2 \"is\" 3}])
    >>>[{\"this\" 1/3, \"is\" 2/5, \"a\" 1, \"test\" 1, \"no\" 1}
        {\"no\" 1, \"this\" 2/3, \"is\" 3/5, \"a\" 3, \"test\" 4}]

  This function is intended to be used after `multiple-seeded-walks`.

  For example:

    (generate-lexicon (multiple-seeded-walks mygraph
                                             25
                                             1000
                                           [\"first\" \"seeds\"]
                                           [\"second\" \"bunch\"]))"
  ([fs] (generate-lexicon fs 3))
  ([fs n]
  (let [c (into {} (filter
                     (fn [x] (< n (val x)))
                     (apply (fn [a b] (merge-with + a b)) fs)))]
  (reduce
    (fn [acc x] (conj acc (merge-with / x (select-keys c (keys x)))))
    []
    fs))))

(defn normalize0
  "Normalize a frequency map so that all the values are between 0 and 1."
  [m]
  (let [vs (vals m)
        a (apply max vs)
        b (apply min vs)
        rng (- a b)]
  (zipmap
    (keys m)
    (map (fn [x] (/ (- x b) rng))  (vals m)))))

(defn normalize
  "Normalize a frequency map so that all the values are between -1 and 1."
  [m]
  (let [vs (vals m)
        a (apply max vs)
        b (apply min vs)
        rng (- a b)]
  (zipmap
    (keys m)
    (map (fn [x] (* (- (/ (- x b) rng) 0.5) 2))  (vals m)))))

(defn -main
  "Empty main function."
  [& args]
  (println "Hello, World!"))
