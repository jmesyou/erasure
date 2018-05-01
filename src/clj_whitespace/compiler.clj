(ns clj-whitespace.compiler
  (:require [clojure.core.match :refer [match]]
            [clj-whitespace.parser :as parser])
  (:gen-class))

(defn compile-helper [cmds prgm labels pc]
  "This is a helper function with full parameters for compilation.
  Included in this function is "
  (if (empty? cmds)
    [prgm labels]
    (let [[n-cmds n-prgm n-labels n-pc] 
      (match [cmds]
        [([[:push a] :pop & xs] :seq)] [xs prgm labels pc]
        [([:swap :swap & xs] :seq)] [xs prgm labels pc]
        [([[:push a] [:push b] :swap & xs] :seq)] [xs (concat prgm [[:push b] [:push a]]) labels (+ pc 2)]
        [([([:label l] :as x) & xs] :seq)] (if (contains? labels l) 
                                              (throw (Exception. "label already present in global table"))
                                              [xs prgm (conj labels {l pc}) pc])
        [([x & xs] :seq)] [xs (conj prgm x) labels (inc pc)]
        [([:end] :seq)] [(list) (conj prgm :end) labels (-1)])]
      (recur n-cmds n-prgm n-labels n-pc))))

(defn compile-tokens [cmds] (compile-helper cmds [] {} 0))

(defn compile-program [s & {:keys [mode] :or {mode :string}}]
  (case mode
    :commands (compile-tokens s)
  (let [tokens (parser/parse s :mode mode)] 
    (compile-tokens tokens))))