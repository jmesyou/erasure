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
  (def instruction-list (new InstructionList))
  (def constant-pool (new ConstantPoolGen))
  (def class-gen (new ClassGen "HelloWorld" "java.lang.Object" "<generated>" (bit-or Const/ACC_PUBLIC Const/ACC_SUPER) nil))
  (def method-gen 
    (new MethodGen 
      (bit-or Const/ACC_STATIC Const/ACC_PUBLIC)
      Type/VOID
      (into-array Type [(new ArrayType Type/STRING 1)])
      (into-array String ["argv"])
      "main"
      "Helloworld"
      instruction-list
      constant-pool))
  )