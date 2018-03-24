(ns clj-whitespace.parser
    (:require [clojure.core.match :refer [match]])
    (:gen-class))

(def parse-stack-cmds)
(def parse-flow-cmds)
(def parse-heap-cmds)
(def parse-arithmetic-cmds)
(def parse-io-cmds)
(def parse-parameter)

(defn read-tokens-from-file [f] (re-seq #"[ \t\n]" (slurp f)))

(defn parse [stream]
    (match [stream]
        [([" " & xs] :seq)]       (parse-stack-cmds xs)
        [(["\t" " " & xs] :seq)]  (parse-arithmetic-cmds xs)
        [(["\t" "\t" & xs] :seq)] (parse-heap-cmds)
        [(["\n" & xs] :seq)]      (parse-flow-cmds xs)
        [(["\t" "\n" & xs] :seq)] (parse-io-cmds xs)
        :else (throw (Exception. "unexpected token encountered while determining next op"))))

(defn parse-stack-cmds [stream] 
    (match [stream]
        [([" " & xs] :seq)] (let [[val rest] (parse-parameter xs)]
                                (let [[sign & num] val]
                                    (def sign' (if (= "0" sign) 1 (-1)))
                                    (def num' (Integer/parseInt (apply str num) 2))
                                    (def number (* sign' num'))
                                    (cons '("PUSH" number) (parse rest))))
        [(["\n" " " & xs] :seq)] (cons "DUP" (parse xs))
        [(["\n" "\t" & xs] :seq)] (cons "SWAP" (parse xs))
        [(["\n" "\n" & xs] :seq)] (cons "POP" (parse xs))
        :else (throw (Exception. "unexpected token encountered while parsing stack op"))))

(defn parse-arithmetic-cmds [stream] 
    (match [stream]
        [([" " " " & xs] :seq)]  (cons "ADD" (parse xs))
        [([" " "\t" & xs] :seq)] (cons "SUB" (parse xs))
        [([" " "\n" & xs] :seq)] (cons "MUL" (parse xs))
        [(["\t" " " & xs] :seq)] (cons "DIV" (parse xs))
        [(["\t" "\t" & xs] :seq)] (cons "MOD" (parse xs))
        :else (throw (Exception. "unexpected token encountered while parsing arith op"))))

(defn parse-heap-cmds [stream] 
    (match [stream]
        [([" " & xs] :seq)]  (cons "STORE" (parse xs))
        [(["\t" & xs] :seq)] (cons "LOAD" (parse xs))
        :else (throw (Exception. "unexpected newline encountered while parsing heap op"))))

(defn parse-flow-cmds [stream] 
    (match [stream]
        [([" " " " & xs] :seq)] (let [[val rest] (parse-parameter xs)] (cons '("LABEL" val) (parse rest)))
        [([" " "\t" & xs] :seq)] (let [[val rest] (parse-parameter xs)] (cons '("CALL" val) (parse rest)))
        [([" " "\n" & xs] :seq)] (let [[val rest] (parse-parameter xs)] (cons '("JUMP" val) (parse rest)))
        [(["\t" " " & xs] :seq)] (let [[val rest] (parse-parameter xs)] (cons '("JUMP-IF-ZERO" val) (parse rest)))
        [(["\t" "\t" & xs] :seq)] (let [[val rest] (parse-parameter xs)] (cons '("JUMP-IF-NEG" val) (parse rest)))
        [(["\t" "\n" & xs] :seq)] (cons "RETURN" (parse xs))
        [(["\n" "\n"] :seq)] (cons "END" (sequence nil))
        :else (throw (Exception. "unexpected token encountered with parsing flow control op"))))

(defn parse-io-cmds [stream] 
    (match [stream]
        [([" " " " & xs] :seq)] (cons "DISPLAY-CHAR" (parse xs))
        [([" " "\t" & xs] :seq)] (cons "DISPLAY-INT" (parse xs))
        [(["\t" " " & xs] :seq)] (cons "READ-CHAR" (parse xs))
        [(["\t" "\t" & xs] :seq)] (cons "READ-INT" (parse xs))
        :else (throw (Exception. "unexpected newline encountered while parsing io op"))))

(defn parse-parameter [stream]
    (let [[x xs] (split-with (partial not= "\n") stream)] 
        (if (< (count x) 2) (throw (Exception. "malformed parameter before linefeed")))
        (def values (map (fn [chr] (if (= chr " ") "0" "1")) x))
        (def xs' (drop 1 xs))
        [values xs']))

