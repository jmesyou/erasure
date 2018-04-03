(ns clj-whitespace.compiler
    (:require [clojure.core.match :refer [match]])
    (:require [clj-whitespace.parser :as parser])
    (:gen-class))

(defn compile-program [cmds prgm labels]
    (if (empty? cmds)
        [prgm labels]
        (let [[n-cmds n-prgm n-labels] 
              (match [cmds]
                [([[:push a] :pop & xs] :seq)] [xs prgm labels]
                [([[:push a] [:push b] :swap & xs] :seq)] [xs (concat prgm [[:push b] [:push a]]) labels]
                [([([:label l] :as x) & xs] :seq)] (if (contains? labels l) 
                                                       (throw (Exception. "label already present in global table"))
                                                       [xs (conj prgm x) (conj labels {l xs})])
                [([x & xs] :seq)] [xs (conj prgm x) labels]
                [([:end] :seq)] [(list) (conj prgm :end) labels])]
             (recur n-cmds n-prgm n-labels))))

