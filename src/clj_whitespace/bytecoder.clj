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
        Type
        ArrayType
        PUSH)
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

  ; (.append instruction-list (new PUSH constant-pool 4711))
  (.append instruction-list (.createPrintln factory "Hello World!"))
  (.append instruction-list InstructionConstants/RETURN)

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

  (.setMaxStack method-gen)
  (.addMethod class-gen (.getMethod method-gen))
  (.dispose instruction-list)
  (.addEmptyConstructor class-gen Const/ACC_PUBLIC)
  (.dump (.getJavaClass class-gen) "HelloWorld.class")

  )