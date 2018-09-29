(ns clj-seagull.core
  (:require [loom.graph :as loom]
            [clojure.string :as str]
            [clojure.core.reducers :as r]
  (:gen-class)))

(defn cooccurrences
  "Get coccurrences"
  [xs]
  (loop [acc {} 
         xs xs]
    (if (empty? xs)
      acc
      (recur 
        (update acc (first xs) (fn [m] (merge-with + m (frequencies (take 5 (rest xs))))))
        (rest xs)))))
       
(defn txt-to-lex-graph
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
  "Returns a weighted sequence of possible next steps"
  [g n]
  (reduce (fn [acc [n1 n2]] (into acc (repeat (loom.graph/weight g n1 n2) n2))) [] (loom.graph/out-edges g n)))
  
(defn random-walk
  "Randomly walk graph and return the path"
  [g start steps]
  ;; Get random nth from sequence of n possible edges
  ;; something like: (rand-nth (repeat edges by weight))
  (loop [pth [start]]
    (if (= (count pth) steps) pth
      (recur (conj pth (rand-nth (weighted-next-steps g (first pth))))))))

(defn seeded-walk-freqs
  "Get the frequency with which words appear in a number of random walks."
  [g steps walks seeds]
  (r/fold 
    (fn ([]{}) ([acc nxt] (merge-with + acc (frequencies (random-walk g nxt steps)))))
    (reduce (fn [acc nxt] (into acc (repeat walks nxt))) [] seeds)))
    
(defn multiple-seeded-walks
  "Perform seeded random walks on multiple sets of seeds"
  [g steps walks & seeds]
  (map (fn [x] (seeded-walk-freqs g steps walks x)) seeds))

(defn generate-lexicon
  "Generate a contrastive lexicon by juxtiposing multiple seeded random walks"
  [fs]
  (let [c (apply (fn [a b] (merge-with + a b)) fs)]
  (reduce (fn [acc x] (conj acc (merge-with / x c))) [] fs)
))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
