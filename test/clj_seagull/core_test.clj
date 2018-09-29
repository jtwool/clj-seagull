(ns clj-seagull.core-test
  (:require [clojure.test :refer :all]
            [clj-seagull.core :refer :all]
            [loom.graph :as loom]))

(deftest cooccurrences-test
  (testing "Tokens to occurances."
    (is (= 
        (cooccurrences ["a" "b" "c"])
        {"a" {"b" 1 "c" 1} "b" {"c" 1} "c" {}}))))

(deftest text-to-lex-graph
  (testing "Lexical graph making function."
    (is (= 
        (txt-to-lex-graph "a b c d. a b")
        (loom/weighted-graph 
          {"a" {"b" 2 "c" 1 "d" 1} 
           "b" {"c" 1 "d" 1} 
           "c" {"d" 1}
           "d" {}})))))
