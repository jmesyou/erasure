(ns clj-whitespace.interpreter
  (:require [clojure.core.match :refer [match]])
  (:gen-class))


(defn interpret [prgm]
  (let 
    [
    stack (atom '())
    call-stack (atom '())
    heap (atom {})
    pc (atom 0)
    state (atom :continue)
    jump-table (atom {})

    consume (fn [] (let [top-item (peek @stack)] (swap! stack pop) top-item))

    binary-op (fn [op] 
      (let [a (consume) b (consume)]
          (swap! stack conj (op b a))))

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
          (let [val (consume) addr (consume)] (swap! heap conj addr val))       
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
            (swap! heap conj addr keyint))
        [[:call addr]] 
          (do 
            (swap! call-stack conj (inc pc))
            (reset! pc addr))
        [[:jump condition label]] 
          (let 
            [b (case condition
              :eq (if (== (consume) 0) true false)
              :ne (if (< (consume) 0) true false)
              :un true)]
            (when b 
              (reset! state :jump)
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
    (while (= @state :continue)
      (do 
        (exec (prgm @pc))
        (case @state 
          :continue 
            (swap! pc inc) 
          :jump
            (swap! state :continue)
          :halt
            (println ""))))))

   
