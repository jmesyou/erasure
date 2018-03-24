(ns clj-whitespace.core
  (:require [clj-whitespace.parser :as parser])
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

