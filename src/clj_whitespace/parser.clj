(ns (ns clj-whitespace.parser
  (:gen-class)))

(def class-name)

(defn parse-line [x] (re-seq #"[ \t\n]" x))

(defn open-file [f]
    (set class-name f)
    (map parse-line )
    )
