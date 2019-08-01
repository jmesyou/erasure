(ns clj-whitespace.parser
  (:require [clojure.core.match :refer [match]])
  (:require [instaparse.core :as insta :refer [defparser]])
  (:gen-class))

(def whitespacer
  "This is a formal ebnf grammar specified from 
  https://hackage.haskell.org/package/whitespace-0.4/src/docs/tutorial.html" 
  (insta/parser 
    "
    <S> = imp+ ;
    <imp> = <space> stack-manipulation
          | <tab> <space> arithmetic
          | <tab> <tab> heap-access
          | <lf> flow-control
          | <tab> <lf> io ;

    <stack-manipulation> = push | pop | swap | dup ;
    push = <space> number ;
    swap = <lf tab> ; 
    pop  = <lf lf> ; 
    dup  = <lf space> ;
    
    <arithmetic> = add | sub | mul | div | mod ;
    add = <space space> ;
    sub = <space tab> ;
    mul = <space lf> ;
    div = <tab space> ;
    mod = <tab tab> ;

    <heap-access> = store | load ;
    store = <space> ;
    load  = <tab> ;

    <flow-control> = location | call | jmp | jz | jn | ret | end ; 
    location  = <space space> label ;
    call  = <space tab> label ;
    jmp   = <space lf> label ;
    jz    = <tab space> label ;
    jn    = <tab tab> label ;
    ret   = <tab lf> ;
    end   = <lf lf> ;

    <io> = print-char | print-int | read-char | read-int ;
    print-char = <space space> ;
    print-int  = <space tab> ;
    read-char  = <tab space> ;
    read-int   = <tab tab> ;

    number = (tab | space) (tab | space)+ <lf> ;
    label = (tab | space)+ <lf>;
    <space> = \" \" ;
    <tab> = \"\t\" ;
    <lf> = \"\n\" ;
    " :start :S))

(defn tokenize 
  "The tokenize function filters all non-accepted characters 
  from the input string as a prequesite for parsing."
  [s] 
  (apply str (re-seq #"[ \t\n]" s)))

(defn parse 
  "An input string `s` is tokenzied and parsed through the instaparse grammar.
  Then the symbolic Whitespace numbers are transformed into appropriate integers.
  Finally, any integer designating a location in the program is verified to be unique."
  [s]
  (->> 
    (insta/parse 
      whitespacer 
      (tokenize s)) 
    ;; convert whitespace numbers to integers and generalize jump instruction.
    (insta/transform 
      {:number (fn [x & xs] (* (if (= x "\t") (- 1) 1) (Integer/parseUnsignedInt (apply str (replace {"\t" "1" " " "0"} xs)) 2)))
       :label (fn [x & xs] (Integer/parseUnsignedInt (apply str (replace {"\t" "1" " " "0"} (cons x xs))) 2))
       :jz  (fn [x] [:jump :eq x])
       :jn  (fn [x] [:jump :ne x])
       :jmp (fn [x] [:jump :un x])
       })
    ;; assert all labels are distinct
    ((fn [xs] 
      (if (distinct? (filter (fn [x] (match [x] [[:location l]] true :else false)) xs))
        xs 
        (throw (Exception. "duplicate labels present")))))
    ;; flatten the vector
    (map (fn [x]
      (match [x]
        [[y]] y
        :else x)))
    vec))   

      