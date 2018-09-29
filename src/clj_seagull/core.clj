(ns clj-seagull.core
  (:gen-class))

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
  "Turns a text document into a lexical graph"
  [txt]
  ;; co-occurrence map to weighted graph
  ;; sent w/ word tokens to cooccurrence map
  ;; word tokenize sents
  ;; sentence tokenize text
  nil)

(defn random-walk
  "Randomly walk graph and return the path"
  [g start steps]
  ;; Get random nth from sequence of n possible edges
  ;; something like: (rand-nth (repeat edges by weight))
  nil)

(defn seeded-walk-freqs
  "Get the frequency with which words appear in a number of random walks"
  [g seeds steps walks]
  ;; repeat seeds walks times and merge the results  
  nil)

(defn generate-lexicon
  "Generate a contrastive lexicon by juxtiposing two seeded random walks"
  [frqs1 frqs2]
  ;; give each tkn a score = (f1 / f1+f2)
  ;; normalize for -1 to 1
  nil)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
