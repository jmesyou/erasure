(ns clj-whitespace.parser
    (:require [clojure.core.match :refer [match]])
    (:require [clojure.pprint])
    (:gen-class))

(def parse-stack-cmds)
(def parse-flow-cmds)
(def parse-heap-cmds)
(def parse-arithmetic-cmds)
(def parse-io-cmds)
(def parse-parameter)
(def parse)

(defn tokenize-string [s] (replace {"\t" :t, "\n" :n, " " :s} (re-seq #"[ \t\n]" s)))

(defn tokenize-file [f] (tokenize-string (slurp f)))

(defn parse [stream]
    (match [stream]
        [([:s & xs] :seq)]    (parse-stack-cmds xs)
        [([:t :s & xs] :seq)] (parse-arithmetic-cmds xs)
        [([:t :t & xs] :seq)] (parse-heap-cmds xs)
        [([:n & xs] :seq)]    (parse-flow-cmds xs)
        [([:t :n & xs] :seq)] (parse-io-cmds xs)
        :else (throw (Exception. "unexpected token encountered while determining next op"))))

(defn parse-stack-cmds [stream] 
    (match [stream]
        [([:s & xs] :seq)] (let [[val rest] (parse-parameter xs)]
                                (let [[sign & num] val]
                                    (def sign' (if (= "0" sign) 1 (-1)))
                                    (def num' (Integer/parseInt (apply str num) 2))
                                    (def number (* sign' num'))
                                    (cons [:push number] (parse rest))))
        [([:n :s & xs] :seq)] (cons :dup (parse xs))
        [([:n :t & xs] :seq)] (cons :swap (parse xs))
        [([:n :n & xs] :seq)] (cons :pop (parse xs))
        :else (throw (Exception. "unexpected token encountered while parsing stack op"))))

(defn parse-arithmetic-cmds [stream] 
    (match [stream]
        [([:s :s & xs] :seq)] (cons :add (parse xs))
        [([:s :t & xs] :seq)] (cons :sub (parse xs))
        [([:s :n & xs] :seq)] (cons :mul (parse xs))
        [([:t :s & xs] :seq)] (cons :div (parse xs))
        [([:t :t & xs] :seq)] (cons :mod (parse xs))
        :else (throw (Exception. "unexpected token encountered while parsing arith op"))))

(defn parse-heap-cmds [stream] 
    (match [stream]
        [([:s & xs] :seq)] (cons :store (parse xs))
        [([:t & xs] :seq)] (cons :load (parse xs))
        :else (throw (Exception. "unexpected newline encountered while parsing heap op"))))

(defn parse-flow-cmds [stream] 
    (match [stream]
        [([:s :s & xs] :seq)] (let [[val rest] (parse-parameter xs)] (cons [:label (Long/parseLong (apply str val) 2)] (parse rest)))
        [([:s :t & xs] :seq)] (let [[val rest] (parse-parameter xs)] (cons [:call (Long/parseLong (apply str val) 2)] (parse rest)))
        [([:s :n & xs] :seq)] (let [[val rest] (parse-parameter xs)] (cons [:jump (Long/parseLong (apply str val) 2)] (parse rest)))
        [([:t :s & xs] :seq)] (let [[val rest] (parse-parameter xs)] (cons [:jump-if-zero (Long/parseLong (apply str val) 2)] (parse rest)))
        [([:t :t & xs] :seq)] (let [[val rest] (parse-parameter xs)] (cons [:jump-if-neg (Long/parseLong (apply str val) 2)] (parse rest)))
        [([:t :n & xs] :seq)] (cons :return (parse xs))
        [([:n :n & xs] :seq)] (cons :end (sequence nil))
        :else (throw (Exception. "unexpected token encountered with parsing flow control op"))))

(defn parse-io-cmds [stream] 
    (match [stream]
        [([:s :s & xs] :seq)] (cons :print-char (parse xs))
        [([:s :t & xs] :seq)] (cons :print-int (parse xs))
        [([:t :s & xs] :seq)] (cons :read-char (parse xs))
        [([:t :t & xs] :seq)] (cons :read-int (parse xs))
        :else (throw (Exception. "unexpected newline encountered while parsing io op"))))

(defn parse-parameter [stream]
    (let [[x xs] (split-with (partial not= :n) stream)] 
        (if (< (count x) 2) (throw (Exception. "malformed parameter before linefeed")))
        (def values (map (fn [chr] (if (= chr :s) "0"  "1")) x))
        (def xs' (drop 1 xs))
        [values xs']))

(defn parse-string [s] 
    (let [tokens (tokenize-string s)] (apply list(parse tokens))))

(defn parse-file [f] 
    (let [tokens (tokenize-file f)] 
        (def output (apply list (parse tokens)))
        (spit (str f ".clj") (str "'" (with-out-str (clojure.pprint/pprint output))))
        output))