(ns clj-whitespace.runtime
    (:require [clojure.core.match :refer [match]])
    (:import [jline.console ConsoleReader])
    (:gen-class))
    
(defn routine [prgm stack table labels call-stack]
    (if (empty? prgm)
        (do ())
        (let [[n-prgm n-stack n-table n-labels n-call-stack] 
             (match [prgm]
             [([[:push v] & xs] :seq)] [xs (conj stack v) table labels call-stack]
             [([:dup & xs] :seq)] (let [x (peek stack)] [xs (conj stack x) table labels call-stack])
             [([:swap & xs] :seq)] (let [a (first stack) b (second stack)] [xs (concat [b a] (nnext stack)) table labels call-stack])
             [([:pop & xs] :seq)] [xs (pop stack) table labels call-stack] 
             [([:add & xs] :seq)] (let [a (first stack) b (second stack)] [xs (conj (nnext stack) (+ b a)) table labels call-stack])
             [([:sub & xs] :seq)] (let [a (first stack) b (second stack)] [xs (conj (nnext stack) (- b a)) table labels call-stack])
             [([:mul & xs] :seq)] (let [a (first stack) b (second stack)] [xs (conj (nnext stack) (* b a)) table labels call-stack])
             [([:div & xs] :seq)] (let [a (first stack) b (second stack)] [xs (conj (nnext stack) (/ b a)) table labels call-stack])
             [([:mod & xs] :seq)] (let [a (first stack) b (second stack)] [xs (conj (nnext stack) (mod b a)) table labels call-stack])
             [([:print-char & xs] :seq)] (do (print (char (first stack))) [xs stack table labels call-stack])
             [([:print-int & xs] :seq)] (do (print (first stack)) [xs stack table labels call-stack])
             [([(:or :read-char :read-int) & xs] :seq)] (let [keyint (Integer/parseInt (read-line)) addr (first stack)] [xs stack (conj table {addr keyint}) labels call-stack])
             :else (throw (Exception. "[runtime/routine] unexpected or malformed op!")))]
             (recur n-prgm n-stack n-table n-labels n-call-stack))))

(defn exec [prgm labels] (routine prgm '() {} labels nil))