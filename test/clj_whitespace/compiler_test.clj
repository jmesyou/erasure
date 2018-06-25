(ns clj-whitespace.compiler-test
  (:require [clojure.test :refer :all]
            [clj-whitespace.compiler :as compiler]))

(deftest parser-test
  (testing "tests parsing functions"
  	
    (is (= [[[:push 1] :dup :end] {0 2}] (compiler/compile-tokens '([:push 1] :dup [:label 0] :end ) ) ) )
    ; peephole optimization
    (is (= [[:end] {0 0}] (compiler/compile-tokens '([:push 1] :pop [:label 0] :end ) ) ) )

))
