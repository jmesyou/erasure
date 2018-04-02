(ns clj-whitespace.core
  (:require [clj-whitespace.parser :as parser])
  (:require [clj-whitespace.compiler :as compiler])
  (:require [clj-whitespace.programs :as programs])
  (:require [clj-whitespace.runtime :as runtime])
  (:import [jline.console ConsoleReader])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (print "Enter a keystroke: ")
  (flush)
  (let [cr (ConsoleReader.)
        keyint (.readCharacter cr)]
    (println (format "Got %d ('%c')!" keyint (char keyint)))))

