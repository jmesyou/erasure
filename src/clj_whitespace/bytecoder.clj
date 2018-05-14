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
        InstructionConstants
        ObjectType
        Type
        ArrayType
        PUSH
        ALOAD
        ASTORE)
      (org.apache.bcel Const))
    (:gen-class))


(defn -main [& args]
  "This function is the main class"
  (def class-name "HelloWorld")
  (def class-gen (new ClassGen class-name "java.lang.Object" "HelloWorld.class" (bit-or Const/ACC_PUBLIC Const/ACC_SUPER) nil))
  ;(.setMajor class-gen 52)
  ;(.setMinor class-gen 0)
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
      class-name
      instruction-list
      constant-pool))

  
  ;(.append instruction-list (new PUSH constant-pool Type/STRING))
  ;(.append instruction-list (new PUSH constant-pool (new ObjectType "java.lang.Long")))
  ;(.append instruction-list (new PUSH constant-pool (new ObjectType "java.lang.Integer")))
  (.append instruction-list (.createNew factory "java.util.TreeMap"))
  (.append instruction-list InstructionConstants/DUP)
  (.append instruction-list 
    (.createInvoke factory
      "java.util.TreeMap"
      "<init>"
      Type/VOID
      Type/NO_ARGS
      Const/INVOKESPECIAL))
  
  (def heap (.addLocalVariable method-gen "heap" (new ObjectType "java.util.TreeMap") nil nil))
  (def index (.getIndex heap))
  (.setStart heap (.append instruction-list (new ASTORE index)))



  (.append instruction-list 
    (.createFieldAccess factory
      "java.lang.System"
      "out"
      (new ObjectType "java.io.PrintStream")
      Const/GETSTATIC))
  (.append instruction-list (new ALOAD index))
  (.append instruction-list 
    (.createInvoke factory
      "java.util.TreeMap"
      "toString"
      Type/STRING
      Type/NO_ARGS
      Const/INVOKEVIRTUAL))
  

  (.append instruction-list 
    (.createInvoke factory 
      "java.io.PrintStream"
      "print"
      Type/VOID
      (into-array Type [Type/STRING])
      Const/INVOKEVIRTUAL
      ))
  (.append instruction-list InstructionConstants/RETURN)



  (.setMaxStack method-gen)
  (.addMethod class-gen (.getMethod method-gen))
  (.dispose instruction-list)
  (.addEmptyConstructor class-gen Const/ACC_PUBLIC)
  (.dump (.getJavaClass class-gen) "HelloWorld.class")

  )
