(ns clj-whitespace.compiler
    (:require [clojure.core.match :refer [match]])
    (:require [clj-whitespace.parser :as parser])
    (:gen-class))

(defn compile-program [cmds program labels]
    (match [cmds]
        [([([:label l] :as x) & xs] :seq)] (if (contains? labels l) 
                                        ((throw (Exception. "label already present in global table")))
                                        (recur xs (conj program x) (conj labels {l xs})))
        [([x & xs] :seq)] (recur xs (conj program x) labels)
        :else [program labels]
        ))

