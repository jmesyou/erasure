(ns clj-whitespace.parser-test
  (:require [clojure.test :refer :all]
            [clj-whitespace.parser :as parser]))

(deftest tokenize-test
  (testing "tests tokenization function"
    (is (= '(:s :t :n) (parser/tokenize " \t\n")))
    (is (= '(:s :t :n) (parser/tokenize "err \t\n")))
    (is (= '(:s :t :n) (parser/tokenize " \t\nerr")))
  ))
  
(deftest parser-test
  (testing "tests parsing function"
    (is (= '([:push 1] :end) (parser/parse-tokens '(:s :s :s :t :n :n :n :n))))
    (is (= '([:push -1] :end) (parser/parse-tokens '(:s :s :t :t :n :n :n :n))))
  ))