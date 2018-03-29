(ns clj-whitespace.compiler
    (:require [clojure.core.match :refer [match]])
    (:require [clj-whitespace.parser :as parser])
    (:gen-class))

(defn compile [cmds program labels]
    (match [cmds]
        [(['("LABEL" l) & xs] :seq)] (recur (xs) (program) (conj labels {l (lazy-seq xs)}))
        [([x & xs] :seq)] (recur (xs) (conj x program) (labels))
        :else [program labels]
        ))

