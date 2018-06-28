(ns clj-whitespace.stackmachine
  (:require [clojure.core.match :refer [match]])
  (:gen-class))


(defn interpret 
  "This function initiates a clean state for the stack machine and 
  executes the program denoted by `prgm`, a vector of instruction keywords and
  their corresponding arguments."
  [prgm]
  (let 
    [
    ;; machine state variables
    stack (atom '())
    call-stack (atom '())
    heap (atom {})
    pc (atom 0)
    state (atom :continue)
    jump-table (atom {})

    ;; custom pop and return function
    consume (fn [] (let [top-item (peek @stack)] (swap! stack pop) top-item))

    ;; FIXME: this should be a macro
    ;; generic binary operation function
    binary-op (fn [op] 
      (let [a (consume) b (consume)]
          (swap! stack conj (op b a))))
    
    ;; execute (1) instruction with side effects on the 
    ;; state of the machine.
    exec (fn [cmd] 
      (match [cmd]
        [[:push val]]
          (swap! stack conj val)
        [:dup] 
          (swap! stack conj (peek @stack))
        [:swap] 
          (let 
            [a (consume)
             b (consume)]
            (swap! stack conj a)
            (swap! stack conj b))
        [:pop] 
          (swap! stack pop) 
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
          (let [val (consume) addr (consume)] (swap! heap conj {addr val}))       
        [:load] 
          (let [
              addr (consume)
              val (get @heap addr 0)] 
              (swap! stack conj val))
        [:print-char] 
          (do (print (char (consume))) (flush))
        [:print-int] 
          (do (print (consume)) (flush))
        [:read-char] 
          (let [keyint (.read System/in) addr (consume)] 
            (swap! heap conj {addr keyint}))
        ;; TODO: add read-int
        [[:location loc]]
          (reset! state :continue)
        [[:call addr]] 
          (do 
            (swap! call-stack conj (inc pc))
            (reset! pc addr))
        ;; generic jump instruction
        [[:jump condition label]] 
          (let 
            ;; decide if a jump should occur
            [b (case condition
              :eq (if (== (consume) 0) true false)
              :ne (if (< (consume) 0) true false)
              :un true)]
            (when b 
              (reset! state :jump)
              ;; if this is the 1st time jumping to a label,
              ;; cache the corresponding address in the jump table, otherwise fetch 
              ;; the address from the jump table
              (if (contains? @jump-table label)
                (reset! pc (@jump-table label))
                (let
                  [pc+ (.indexOf prgm [:location label])]
                  (swap! jump-table conj {label pc+})
                  (reset! pc pc+)))))
        [:ret] 
          (let [pc+ (first @call-stack)]
            (swap! call-stack pop)
            (reset! pc pc+))
        [:end]  (reset! state :halt)
      ))
     ]
    ;; infinite loop breaking on the halt state.
    (loop []
      (do 
        (exec (prgm @pc))
        (case @state 
          :continue 
            (do
              (swap! pc inc)
              (recur)) 
          :jump
            (recur)
          :halt
            (println ""))))))

   
