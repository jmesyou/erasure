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
  (testing "tests parsing functions"
    ; (is (= '(:command :end) (parser/parse-tokens '(whitespace_program  :n :n :n))))
    ; stack manipulation
    (is (= '([:push 1] :end) (parser/parse-tokens '(:s :s :s :t :n :n :n :n))))
    (is (= '([:push -1] :end) (parser/parse-tokens '(:s :s :t :t :n :n :n :n))))
    (is (= '(:dup :end) (parser/parse-tokens '(:s :n :s :n :n :n))))
    (is (= '(:swap :end) (parser/parse-tokens '(:s :n :t :n :n :n))))
    (is (= '(:pop :end) (parser/parse-tokens '(:s :n :n :n :n :n))))
    ;exception
    (is (thrown? Exception (parser/parse-tokens '(:s :s :s :t :n :n :n))))
    ; arithmetic
    (is (= '(:add :end) (parser/parse-tokens '(:t :s :s :s :n :n :n))))
    (is (= '(:sub :end) (parser/parse-tokens '(:t :s :s :t :n :n :n))))
    (is (= '(:mul :end) (parser/parse-tokens '(:t :s :s :n :n :n :n))))
    (is (= '(:div :end) (parser/parse-tokens '(:t :s :t :s :n :n :n))))
    (is (= '(:mod :end) (parser/parse-tokens '(:t :s :t :t :n :n :n))))
    ;exception
    (is (thrown? Exception (parser/parse-tokens '(:t :s :n :t :n :n :n))))
    ; heap access
    (is (= '(:store :end) (parser/parse-tokens '(:t :t :s :n :n :n))))
    (is (= '(:load :end) (parser/parse-tokens '(:t :t :t :n :n :n))))
    ;exception
    (is (thrown? Exception (parser/parse-tokens '(:t :t :n :n :n :n))))
    ; flow control
    (is (= '([:label 1] :end) (parser/parse-tokens '(:n :s :s :t :n :n :n :n))))
    (is (= '([:call 0] :end) (parser/parse-tokens '(:n :s :t :s :n :n :n :n))))
    (is (= '([:jmp 0] :end) (parser/parse-tokens '(:n :s :n :s :n :n :n :n))))
    (is (= '([:jz 0] :end) (parser/parse-tokens '(:n :t :s :s :n :n :n :n))))
    (is (= '([:jn 0] :end) (parser/parse-tokens '(:n :t :t :s :n :n :n :n))))
    (is (= '(:ret :end) (parser/parse-tokens '(:n :t :n :n :n :n))))
    (is (= '(:end) (parser/parse-tokens '(:n :n :n))))
    ;exception
    (is (thrown? Exception (parser/parse-tokens '(:n :t :t :n :n :n))))
    ; i/o
    (is (= '(:printc :end) (parser/parse-tokens '(:t :n :s :s :n :n :n))))
    (is (= '(:printi :end) (parser/parse-tokens '(:t :n :s :t :n :n :n))))
    (is (= '(:readc :end) (parser/parse-tokens '(:t :n :t :s :n :n :n))))
    (is (= '(:readi :end) (parser/parse-tokens '(:t :n :t :t :n :n :n))))
    ;exception
    (is (thrown? Exception (parser/parse-tokens '(:t :t :n :n :n :n))))
))
