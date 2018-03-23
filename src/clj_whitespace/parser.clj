(ns clj-whitespace.parser
    (:require [clojure.core.match :refer [match]])
    (:gen-class))

(def parse-stack-cmds)
(def parse-flow-cmds)
(def parse-heap-cmds)
(def parse-arithmetic-cmds)
(def parse-parameter)

(defn read-tokens-from-file [f] (re-seq #"[ \t\n]" (slurp f)))

(defn parse [stream]
    (match [stream]
        [([" " & xs] :seq)]       (parse-stack-cmds xs)
        [(["\t" " " & xs] :seq)]  (parse-arithmetic-cmds xs)
        [(["\t" "\t" & xs] :seq)] ()
        [(["\n" & xs] :seq)]      (parse-flow-cmds xs)
        [(["\t" "\n" & xs] :seq)] ()
        :else (throw (Exception. "unexpected token encountered while determining next op"))))

(defn parse-stack-cmds [stream] 
    (match [stream]
        [([" " & xs] :seq)] (let [[val rest] (parse-parameter xs)]
                                (let [[sign & num] val]
                                    (def sign' (if (= "0" sign) 1 (-1)))
                                    (def num' (Integer/parseInt (apply str num) 2))
                                    (def number (* sign' num'))
                                    (cons "PUSH" (cons number (parse rest)))))
        [(["\n" " " & xs] :seq)] (cons "DUP" (parse xs))
        [(["\n" "\t" & xs] :seq)] (cons "SWAP" (parse xs))
        [(["\n" "\n" & xs] :seq)] (cons "POP" (parse xs))
        :else (throw (Exception. "unexpected token encountered while parsing stack op"))))

(defn parse-arithmetic-cmds [stream] (
    match [stream]
        [([" " " " & xs] :seq)]  (cons "ADD" (parse xs))
        [([" " "\t" & xs] :seq)] (cons "SUB" (parse xs))
        [([" " "\n" & xs] :seq)] (cons "MUL" (parse xs))
        [(["\t" " " & xs] :seq)] (cons "DIV" (parse xs))
        [(["\t" "\t" & xs] :seq)] (cons "MOD" (parse xs))
))
(defn parse-flow-cmds [stream] 
    (match [stream]
        [(["\n" "\n"] :seq)] (sequence nil)
        :else (throw (Exception. "unexpected token encountered with parsing flow control op"))))


(defn parse-parameter [stream]
    (let [[x xs] (split-with (partial not= "\n") stream)] 
        (if (< (count x) 2) (throw (Exception. "malformed parameter before linefeed")))
        (def values (map (fn [chr] (if (= chr " ") "0" "1")) x))
        (def xs' (drop 1 xs))
        [values xs']))

