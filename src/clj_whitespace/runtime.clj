(ns clj-whitespace.runtime
    (:require [clojure.core.match :refer [match]])
    (:require [clj-whitespace.compiler :as compiler])
    (:gen-class))

(defn routine [prgm stack table labels call-stack pc]
    (if (== pc (- 1))
        (do ())
        (let [[n-pc n-stack n-table n-labels n-call-stack] 
             (match [(get prgm pc)]
             [[:push v]] [(inc pc) (conj stack v) table labels call-stack]
             [:dup] (let [x (first stack)] [(inc pc) (conj stack x) table labels call-stack])
             [:swap] (let [a (first stack) b (second stack)] [(inc pc) (concat [b a] (nnext stack)) table labels call-stack])
             [:pop] [(inc pc) (next stack) table labels call-stack] 
             [:add] (let [a (first stack) b (second stack)] [(inc pc) (conj (nnext stack) (+ b a)) table labels call-stack])
             [:sub] (let [a (first stack) b (second stack)] [(inc pc) (conj (nnext stack) (- b a)) table labels call-stack])
             [:mul] (let [a (first stack) b (second stack)] [(inc pc) (conj (nnext stack) (* b a)) table labels call-stack])
             [:div] (let [a (first stack) b (second stack)] [(inc pc) (conj (nnext stack) (/ b a)) table labels call-stack])
             [:mod] (let [a (first stack) b (second stack)] [(inc pc) (conj (nnext stack) (mod b a)) table labels call-stack])
             [:store] (let [val (first stack) addr (second stack)] [(inc pc) stack (conj! table {addr val}) labels call-stack])
             [:load] (let [addr (first stack)] [(inc pc) (conj stack (get table addr 0)) table labels call-stack])
             [:print-char] (do (print (char (first stack))) [(inc pc) stack table labels call-stack])
             [:print-int] (do (print (first stack)) [(inc pc) stack table labels call-stack])
             [(:or :read-char :read-int)] (let [keyint (Integer/parseInt (read-line)) addr (first stack)] [(inc pc) stack (conj! table {addr keyint}) labels call-stack])
             [[:call l]] [(get labels l (- 1)) stack table labels (conj call-stack (inc pc))]
             [[:jump l]] [(get labels l (- 1)) stack table labels call-stack]
             [[:jump-if-zero l]] (if (== (first stack) 0) [(get labels l (- 1)) stack table labels call-stack] [(inc pc) stack table labels call-stack])
             [[:jump-if-neg l]] (if (< (first stack) 0) [(get labels l (- 1)) stack table labels call-stack] [(inc pc) stack table labels call-stack])
             [:ret] [(first call-stack) stack table labels (next call-stack)]
             [:end] [(- 1) nil nil nil nil]
             :else (throw (Exception. "[runtime/routine] unexpected or malformed op!")))]
             (recur prgm n-stack n-table n-labels n-call-stack n-pc))))

(defn main-routine [prgm labels] (routine prgm '() (transient {}) labels '() 0))

(defn exec [s & {:keys [mode] :or {mode :string}}] 
    (let [[programs labels] (compiler/compile-program s :mode mode)]
        (main-routine programs labels)))
