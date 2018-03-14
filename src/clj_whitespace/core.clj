(ns clj-whitespace.core
  (:require [clj-whitespace.parser :as parser])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (def test1 '(" " " " " " "\t" " " "\n" " " "\n" "\t" "\n" "\n" "\n"))
  (prn test1)
  (println (parser/parse test1)))

