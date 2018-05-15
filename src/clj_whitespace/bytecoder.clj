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

(def heap-type-string "java.util.TreeMap")
(def heap-type (new ObjectType heap-type-string))
(def init-heap)

(def class-name "HelloWorld")

(def class-gen (new ClassGen nil "java.lang.Object" "HelloWorld.class" (bit-or Const/ACC_PUBLIC Const/ACC_SUPER) nil))
(def instruction-list (new InstructionList))
(def constant-pool (.getConstantPool class-gen))
(def factory (new InstructionFactory class-gen))

(def method-gen 
  (new MethodGen 
    (bit-or Const/ACC_STATIC Const/ACC_PUBLIC)
    Type/VOID
    (into-array Type [(new ArrayType Type/STRING 1)])
    (into-array String ["argv"])
    "main"
    nil
    instruction-list
    constant-pool))

(defn -main [& args]
  "This function is the main class"


  ;(.setMajor class-gen 52)
  ;(.setMinor class-gen 0)
  



  (.append instruction-list 
    (.createFieldAccess factory
      "java.lang.System"
      "out"
      (new ObjectType "java.io.PrintStream")
      Const/GETSTATIC))
  
  (let [[idx ilist] (init-heap method-gen factory)]
    (.append instruction-list ilist))
  (.append instruction-list InstructionConst/RETURN)


  (.setMaxStack method-gen)
  (.addMethod class-gen (.getMethod method-gen))
  (.dispose instruction-list)
  (.dump (.getJavaClass class-gen) "HelloWorld.class")

  )

(defn init-heap [method-gen factory] 
  (let 
    [instruction-list (new InstructionList)
     heap (.addLocalVariable method-gen "heap" heap-type nil nil)]
    (.append instruction-list (.createNew factory heap-type-string))
    (.append instruction-list InstructionConst/DUP)
    (.append instruction-list 
      (.createInvoke factory
        heap-type-string
        "<init>"
        Type/VOID
        Type/NO_ARGS
        Const/INVOKESPECIAL))
      
    (def index (.getIndex heap))
    (.setStart heap (.append instruction-list (new ASTORE index)))
    [index instruction-list]))

(defn heap-store [heap-idx factory]
  (new InstructionList
    (into-array Instruction
      (new ALOAD heap-idx)
      (.createInvoke factory
        heap-type-string
        "put"
        Type/INT
        (into-array Type [Type/LONG Type/INT])))))

(defn print-int [] 
  (.createInvoke factory 
      "java.io.PrintStream"
      "print"
      Type/VOID
      (into-array Type [Type/STRING])
      Const/INVOKEVIRTUAL
      ))

(defn instruction-to-bytecode [const-pool heap-idx instr] 
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
    [:store] 
    [:load] 
    [:printc] 
    [:printi] 
    [(:or :readc :readi)] 
    [[:call l]] 
    [[:jmp l]] 
    [[:jz l]] 
    [[:jn l]] 
    [:ret] 
    [:end] 
    :else (throw (Exception. "[runtime/routine] unexpected or malformed op!")))
    )