(ns clj-whitespace.runtime
    (:require [clojure.core.match :refer [match]])
    (:gen-class))

(defn routine [prgm stack table labels call-stack]
    (if (empty? prgm)
        (do ())
        (match [prgm]
            [([[:push v] & xs] :seq)] (routine xs (conj stack v) table labels call-stack)
            [([:dup & xs] :seq)] (let [x (peek stack)] (routine xs (conj stack x) table labels call-stack))
            [([:swap & xs] :seq)] (let [a (first stack) b (second stack)] (routine xs (concat [b a] (nnext stack)) table labels call-stack))
            [([:pop & xs] :seq)] (routine xs (pop stack) table labels call-stack) 
            [([:add & xs] :seq)] (let [a (first stack) b (second stack)] (routine xs (conj (nnext stack) (+ b a)) table labels call-stack))
            [([:sub & xs] :seq)] (let [a (first stack) b (second stack)] (routine xs (conj (nnext stack) (- b a)) table labels call-stack))
            [([:mul & xs] :seq)] (let [a (first stack) b (second stack)] (routine xs (conj (nnext stack) (* b a)) table labels call-stack))
            [([:div & xs] :seq)] (let [a (first stack) b (second stack)] (routine xs (conj (nnext stack) (/ b a)) table labels call-stack))
            [([:mod & xs] :seq)] (let [a (first stack) b (second stack)] (routine xs (conj (nnext stack) (mod b a)) table labels call-stack))
            [([:print-char & xs] :seq)] (do (print (char (first stack))) (routine xs stack table labels call-stack))
            [([:print-int & xs] :seq)] (do (print (first stack)) (routine xs stack table labels call-stack))
            :else (throw (Exception. "[runtime/routine] unexpected or malformed op!")))))

(defn exec [prgm labels] (routine prgm '() {} labels nil))