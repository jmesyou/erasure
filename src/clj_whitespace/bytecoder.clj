(ns clj-whitespace.bytecoder
    (:require [clj-whitespace.parser :as parser]
              [clj-whitespace.compiler :as compiler]
              [clj-whitespace.runtime :as runtime]
              [clojure.string :as string]
              [clojure.tools.cli :refer [parse-opts]])
    (:import 
      (org.apache.bcel.generic 
        ClassGen 
        MethodGen
        ConstantPoolGen
        InstructionList
        InstructionFactory
        InstructionConst
        ObjectType
        Type
        ArrayType
        PUSH
        ALOAD
        ASTORE)
      (org.apache.bcel Const))
    (:gen-class))
(def reader-type-string "java.util.Scanner")
(def reader-type (new ObjectType reader-type-string))
    
(def heap-type-string "java.util.TreeMap")
(def heap-type (new ObjectType heap-type-string))

(defn to-bytecode [class-name instructions] 
  (let 
    [
      class-gen 
        (new ClassGen nil "java.lang.Object" (str class-name ".class") (bit-or Const/ACC_PUBLIC Const/ACC_SUPER) nil))
      instruction-list 
        (new InstructionList)
      constant-pool 
        (.getConstantPool class-gen)
      factory 
        (new InstructionFactory class-gen)
      method-gen 
        (new MethodGen 
          (bit-or Const/ACC_STATIC Const/ACC_PUBLIC)
          Type/VOID
          (into-array Type [(new ArrayType Type/STRING 1)])
          (into-array String ["argv"])
          "main"
          nil
          instruction-list
          constant-pool)

      heap-idx 
        (let 
          [heap (.addLocalVariable method-gen "heap" heap-type nil nil)] 
          (.getIndex heap))
      
      scan-idx 
        (let 
          [scan (.addLocalVariable method-gen "heap" reader-type nil nil)] 
          (.getIndex scan))

      access-system-in (fn []
        (.createFieldAccess factory
          "java.lang.System"
          "in"
          (new ObjectType "java.io.InputStream")
          Const/GETSTATIC))
      
      access-system-out (fn []
        (.createFieldAccess factory
          "java.lang.System"
          "out"
          (new ObjectType "java.io.PrintStream")
          Const/GETSTATIC))
          
      init-scan (fn []
        (.append instruction-list (.createNew factory reader-type-string))
        (.append instruction-list InstructionConst/DUP)
        (.append instruction-list 
          (access-system-in))
        (.append instruction-list 
          (.createInvoke factory
            reader-type-string
            "<init>"
            Type/VOID
            (into-array Type [(new ObjectType "java.io.InputStream")])
            Const/INVOKESPECIAL))
        (.setStart scan (.append instruction-list (new ASTORE scan-idx))))
        
      init-heap (fn []
        (.append instruction-list (.createNew factory heap-type-string))
        (.append instruction-list InstructionConst/DUP)
        (.append instruction-list 
          (.createInvoke factory
            heap-type-string
            "<init>"
            Type/VOID
            Type/NO_ARGS
            Const/INVOKESPECIAL))
        (.setStart heap (.append instruction-list (new ASTORE heap-idx))))

      heap-store (fn []
        (new InstructionList
          (into-array Instruction
            (new ALOAD heap-idx)
            (.createInvoke factory
              heap-type-string
              "put"
              Type/INT
              (into-array Type [Type/LONG Type/INT])))))

      heap-load (fn []
        (new InstructionList
          (into-array Instruction
            (new ALOAD heap-idx)
            (.createInvoke factory
              heap-type-string
              "get"
              Type/INT
              (into-array Type [Type/LONG])))))
        )

      print-int (fn [] 
        (new InstructionList 
          (into-array Instruction 
            [ (access-system-out)
              (.createInvoke factory 
                "java.io.PrintStream"
                "print"
                Type/VOID
                (into-array Type [Type/INT])
                Const/INVOKEVIRTUAL)])

      print-char (fn []
        (new InstructionList 
          (into-array Instruction 
            [ (access-system-out)
              (.createInvoke factory 
                "java.io.PrintStream"
                "print"
                Type/VOID
                (into-array Type [Type/CHAR])
                Const/INVOKEVIRTUAL)])
      
      read-int (fn [] 
        (new InstructionList 
          (into-array Instruction 
            [ (new ALOAD scan-idx)
              (.createInvoke factory 
                "java.util.Scanner"
                "nextInt"
                Type/INT
                Type/NO_ARGS)
                Const/INVOKEVIRTUAL)]))
        
      read-char (fn [] 
        (new InstructionList 
          (into-array Instruction 
            [ (access-system-in)
              (.createInvoke factory 
                "java.io.InputStream"
                "read"
                Type/INT
                Type/NO_ARGS)
                Const/INVOKEVIRTUAL)]))
      

      instruction-to-bytecode [const-pool heap-idx instr] 
        (match [instr]
          [[:push val]] (new PUSH const-pool val)
          [:pop]  InstructionConst/POP
          [:dup]  InstructionConst/DUP
          [:swap] InstructionConst/SWAP
          [:add]  InstructionConst/IADD
          [:sub]  InstructionConst/ISUB
          [:mul]  InstructionConst/IMUL
          [:div]  InstructionConst/IDIV
          [:mod] 
            (new InstructionList 
              (into-array Instruction 
                [InstructionConst/DUP2
                 InstructionConst/DUP2
                 InstructionConst/IDIV
                 InstructionConst/IMUL
                 InstructionConst/ISUB]))
          [:store] (heap-store)
          [:load]  (heap-load)
          [:printc] (print-char)
          [:printi] (print-int)
          [:readi] (read-int)
          [:readc] (read-char)
          [[:call l]] [(new JSR) l]
          [[:jmp l]] [(new GOTO) l]
          [[:jz l]] [(new IFEQ) l]
          [[:jn l]] [(new IFNE) l] 
          [:ret] []
          [:end] 
          :else (throw (Exception. "[runtime/routine] unexpected or malformed op!")))
    ]
    
    (.append instruction-list 
      

    (init-heap)
    (.append instruction-list InstructionConst/RETURN)

    (.setMaxStack method-gen)
    (.addMethod class-gen (.getMethod method-gen))
    (.dispose instruction-list)
    (.dump (.getJavaClass class-gen) (str class-name ".class"))
)


(defn -main [& args] 
  
  )
