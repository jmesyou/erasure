(ns clj-whitespace.parser
  (:require [clojure.core.match :refer [match]])
  (:require [clojure.pprint])
  (:gen-class))

(def parse-stack-tokens)
(def parse-flow-tokens)
(def parse-heap-tokens)
(def parse-arithmetic-tokens)
(def parse-io-tokens)
(def parse-parameter)
(def parse-label)
(def parse-tokens)

(defn tokenize [s & {:keys [mode] :or {mode :string}}] 
  "This function is the whole tokenizer.
  Detailed description of the lexer is fully described
  in the report."
  (case mode 
    :string (replace {"\t" :t, "\n" :n, " " :s} (re-seq #"[ \t\n]" s))
    :file (tokenize (slurp s) :mode :string)))

(defn parse-tokens [stream]
  "The start rule for parsing.
  A full explanation of its functionality can be found in 
  the report."
  (match [stream]
    [([:s & xs] :seq)]    (parse-stack-tokens xs)
    [([:t :s & xs] :seq)] (parse-arithmetic-tokens xs)
    [([:t :t & xs] :seq)] (parse-heap-tokens xs)
    [([:n & xs] :seq)]    (parse-flow-tokens xs)
    [([:t :n & xs] :seq)] (parse-io-tokens xs)
    :else (throw (Exception. "unexpected token encountered while determining next op"))))

(defn parse-stack-tokens [stream] 
  "This function parses 1 stack op at the head of `stream`.
  "
  (match [stream]
    [([:s & xs] :seq)] (let [[val rest] (parse-parameter xs)]
                          (let [[sign & num] val]
                          (def sign' (if (= "0" sign) 1 (- 1)))
                          (def num' (Integer/parseInt (apply str num) 2))
                          (def number (* sign' num'))
                          (cons [:push number] (parse-tokens rest))))
    [([:n :s & xs] :seq)] (cons :dup (parse-tokens xs))
    [([:n :t & xs] :seq)] (cons :swap (parse-tokens xs))
    [([:n :n & xs] :seq)] (cons :pop (parse-tokens xs))
    :else (throw (Exception. "unexpected token encountered while parsing stack op"))))

(defn parse-arithmetic-tokens [stream] 
  "This function parses 1 arithmetic op at the head of `stream`.
  "
  (match [stream]
    [([:s :s & xs] :seq)] (cons :add (parse-tokens xs))
    [([:s :t & xs] :seq)] (cons :sub (parse-tokens xs))
    [([:s :n & xs] :seq)] (cons :mul (parse-tokens xs))
    [([:t :s & xs] :seq)] (cons :div (parse-tokens xs))
    [([:t :t & xs] :seq)] (cons :mod (parse-tokens xs))
    :else (throw (Exception. "unexpected token encountered while parsing arith op"))))

(defn parse-heap-tokens [stream] 
  "This function parses 1 store/load op at the head of `stream`.
  "
  (match [stream]
    [([:s & xs] :seq)] (cons :store (parse-tokens xs))
    [([:t & xs] :seq)] (cons :load (parse-tokens xs))
    :else (throw (Exception. "unexpected newline encountered while parsing heap op"))))

(defn parse-flow-tokens [stream] 
  "This function parses 1 flow control op at the head of `stream`."
  (match [stream]
    [([:s :s & xs] :seq)] (let [[val rest] (parse-label xs)] (cons [:label val] (parse-tokens rest)))
    [([:s :t & xs] :seq)] (let [[val rest] (parse-label xs)] (cons [:call val] (parse-tokens rest)))
    [([:s :n & xs] :seq)] (let [[val rest] (parse-label xs)] (cons [:jmp val] (parse-tokens rest)))
    [([:t :s & xs] :seq)] (let [[val rest] (parse-label xs)] (cons [:jz val] (parse-tokens rest)))
    [([:t :t & xs] :seq)] (let [[val rest] (parse-label xs)] (cons [:jn val] (parse-tokens rest)))
    [([:t :n & xs] :seq)] (cons :ret (parse-tokens xs))
    [([:n :n & xs] :seq)] (cons :end (sequence nil))
    :else (throw (Exception. "unexpected token encountered with parsing flow control op"))))

(defn parse-io-tokens [stream] 
  "This function parses 1 io op at the head of `stream`."
  (match [stream]
    [([:s :s & xs] :seq)] (cons :printc (parse-tokens xs))
    [([:s :t & xs] :seq)] (cons :printi (parse-tokens xs))
    [([:t :s & xs] :seq)] (cons :readc (parse-tokens xs))
    [([:t :t & xs] :seq)] (cons :readi (parse-tokens xs))
    :else (throw (Exception. "unexpected newline encountered while parsing io op"))))

(defn parse-parameter [stream]
  "This function parses a binary number of length (>=1)
  The 1st symbol (:s | :t) represents (-1 | 1) and the rest
  represents a little endian binary number where (:s | :t)
  represents (0 | 1)"
  (let [[x xs] (split-with (partial not= :n) stream)] 
    (if (< (count x) 2) (throw (Exception. "malformed parameter before linefeed")))
    (def values (map (fn [chr] (if (= chr :s) "0"  "1")) x))
    (def xs' (drop 1 xs))
    [values xs']))

(defn parse-label [stream]
  "This function parses a binary label of length (>=1)."
  (let [[x xs] (split-with (partial not= :n) stream)] 
    (if (< (count x) 1) 
      (do 
        (println x)
        (println xs) 
        (throw (Exception. "malformed label before linefeed"))))
  (def values (map (fn [chr] (if (= chr :s) "0"  "1")) x))
  (def xs' (drop 1 xs))
  [(Long/parseLong (apply str values) 2) xs']))

(defn pretty-intermediate-out [s prgm]
  "This function outputs an intermediate file representation.
  `s` represents the file name of the input source file.
  `prgm` is the parsed source in intermediate code."
  (spit (str s ".clj") (str "'" (with-out-str (clojure.pprint/pprint prgm)))))

(defn parse [s & {:keys [mode intermediate?] :or {mode :string intermediate? false}}]
  "This is the helper function which is exported into all other modules." 
  (case mode
    :string (let [tokens (tokenize s :mode :string)] (apply list (parse-tokens tokens)))
    :file 
      (let [tokens (tokenize s :mode :file)] 
      (def output (apply list (parse-tokens tokens)))
      (if intermediate? (pretty-intermediate-out s output))
       output)))
    

