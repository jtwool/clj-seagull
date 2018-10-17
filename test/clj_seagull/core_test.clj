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

(deftest random-walk-test
  (let [g (txt-to-lex-graph "this is is a test")]
    (testing "Find next steps"
      (is (= (weighted-next-steps g "this")
             ["is" "is" "a" "test"])))
    (testing "Random walk contains start"
      (is true? (contains? (random-walk g "this" 5) "this")))
    (testing "Random walk is of proper length"
      (is (= (count (random-walk g "this" 5)) 5)))))

(deftest seeded-walks-tests
  (let [g (txt-to-lex-graph "this is is a test")
        swf (seeded-walk-freqs g 2 2 ["this" "a"])
        mswf (multiple-seeded-walks g 2 2 ["this" "is"] ["a" "test"])
        kvs {"this" 1 "that" 2 "test" 3}]
    (testing "Return type of seeded walk is same as frequency counts"
      (is (= (type swf) (type kvs)))
      (is (= (type (keys swf)) (type (keys kvs))))
      (is (= (type (vals swf)) (type (vals kvs)))))
    (testing "Multiple seeded walks will return multiple frequency counts"
      (is (= (type mswf) (type (lazy-seq swf swf))))
      (is (= (type (keys mswf)) (type (keys (lazy-seq swf swf)))))
      (is (= (type (vals mswf)) (type (vals (lazy-seq swf swf))))))
    ))

(deftest lexicon-tests
  (let [my-lex (generate-lexicon [{"this" 1, "is" 2, "a" 3, "test" 4}
                                  {"no" 1, "this" 2, "is" 3}] 1)]
  (testing "Lexicons should contain words with frequency >1"
    (is true? (contains? (first my-lex) "this")))
  (testing "Lexicons should not contain words with frequency 1"
    (is false? (contains? (nth my-lex 1) "no")))
))
