(ns clj-whitespace.debug
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
        Type
        ArrayType)
      (org.apache.bcel Const))
    (:gen-class))


(defn -main [& args]
  "This function is the main class"
  (def class-name "HelloWorld")
  (def class-gen (new ClassGen class-name "java.lang.Object" "<generated>" (bit-or Const/ACC_PUBLIC Const/ACC_SUPER) nil))
  (def instruction-list (new InstructionList))
  (def constant-pool (new ConstantPoolGen))
  (def factory (new InstructionFactory class-gen))

  (instruction-list/append (new PUSH constant-pool "Hello World"))
  (instruction-list/append (new PUSH constant-pool 4711))
  (instruction-list/append (factory/createPrintln "Hello World"))

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
  )