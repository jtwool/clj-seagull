(ns clj-seagull.core-test
  (:require [clojure.test :refer :all]
            [clj-seagull.core :refer :all]))

(deftest cooccurrences-test
  (testing "Tokens to occurances."
    (is (= 
        (cooccurrences ["a" "b" "c"])
        {"a" {"b" 1 "c" 1} "b" {"c" 1} "c" {}}))))
