(ns clj-whitespace.runtime
  (:require [clojure.core.match :refer [match]])
  (:require [clj-whitespace.compiler :as compiler])
  (:gen-class))

(defn routine [prgm stack table labels call-stack pc]
  "This function is a helper function for a main rountine.
  It keeps all the variables for the routine as a state, whereas
  a main routine always start with empty values, subroutines do not."
  (if (== pc (- 1))
      (do ())
      (let 
        [[n-pc n-stack n-table n-labels n-call-stack] 
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
        [:store] (let [val (first stack) addr (second stack)] [(inc pc) (nnext stack) (conj! table {addr val}) labels call-stack])
        [:load] (let [addr (first stack)] [(inc pc) (conj (next stack) (get table addr 0)) table labels call-stack])
        [:printc] (do (print (char (first stack))) (flush) [(inc pc) (next stack) table labels call-stack])
        [:printi] (do (print (first stack)) (flush) [(inc pc) (next stack) table labels call-stack])
        [(:or :readc :readi)] (let [keyint (Integer/parseInt (read-line)) addr (first stack)] [(inc pc) (next stack) (conj! table {addr keyint}) labels call-stack])
        [[:call l]] [(get labels l (- 1)) stack table labels (conj call-stack (inc pc))]
        [[:jmp l]] [(get labels l (- 1)) stack table labels call-stack]
        [[:jz l]] (if (== (first stack) 0) [(get labels l (- 1)) (next stack) table labels call-stack] [(inc pc) (next stack) table labels call-stack])
        [[:jn l]] (if (< (first stack) 0) [(get labels l (- 1)) (next stack) table labels call-stack] [(inc pc) (next stack) table labels call-stack])
        [:ret] [(first call-stack) stack table labels (next call-stack)]
        [:end] [(- 1) nil nil nil nil]
        :else (throw (Exception. "[runtime/routine] unexpected or malformed op!")))]
        (recur prgm n-stack n-table n-labels n-call-stack n-pc))))

(defn main-routine [prgm labels]
  "This is a main routine in runtime.
  A main routine is considered the master routine from which
  all subroutines are called."
  (routine prgm '() (transient {}) labels '() 0))


(defn new-routine [prgm]
  (let 
    [stack (atom '())
     call-stack (atom '())
     heap (atom {})
     prgm-counter (atom 0)

     pop-stack (fn [] (let [x (first @stack)] (swap! stack pop) x))

    binary-op (fn [op] 
      (let [a (pop-stack) b (pop-stack)]
          (swap! stack conj (op b a))))

     exec (fn [cmd] 
      (match [cmd]
        [[:push v]]
          (swap! stack conj v)
        [:dup] 
          (swap! stack conj (peek @stack))
        [:swap] 
          (let 
            [a (pop-stack)
             b (pop-stack)]
            (swap! stack conj a)
            (swap! stack conj b))
        [:pop] 
          (swap! stack pop) 
        [:pop2]
          (dotimes [n 2] (swap! stack pop))
        [:add] 
          (binary-op +)
        [:sub]
          (binary-op -)
        [:mul] 
          (binary-op *)
        [:div] 
          (binary-op /)
        [:mod]
          (binary-op [mod])
        [:store] 
          (let [val (pop-stack) addr (pop-stack)] (swap! heap conj addr val))       
        [:load] 
          (let [addr (pop-stack)
                value (get @heap addr 0)] 
              (swap! stack conj value))
        [:printc] 
          (do (print (char (pop-stack))) (flush))
        [:printi] 
          (do (print (pop-stack)) (flush))
        [:readc] 
          (let [keyint (.read System/in) addr (pop-stack)] 
            (swap! heap conj addr keyint))
        [[:call l]] 
          (do 
            (swap! call-stack conj prgm-counter)
            (reset! prgm-counter (- l 1)))
        [[:jmp l]] 
          (reset! prgm-counter (- l 1))
        [[:jz l]] 
          (if (== (pop-stack) 0) (reset! prgm-counter (- l 1)) nil)
        [[:jn l]] 
          (if (< (pop-stack) 0) (reset! prgm-counter (- l 1)) nil)
        [:ret] 
          (let [new-counter (first @call-stack)]
            (swap! call-stack pop)
            (reset! prgm-counter new-counter))
        [:end]  (reset! prgm-counter (- 2))
      ))
     ]
    (while (>= @prgm-counter 0) 
      (exec (prgm @prgm-counter))
      (swap! prgm-counter inc))
    (println "")
    ))

(defn exec [s & {:keys [mode] :or {mode :string}}] 
  (let 
    [[programs labels] (compiler/compile-program s :mode mode)]
    (main-routine programs labels)))

   
