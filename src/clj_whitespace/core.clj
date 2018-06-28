(ns clj-whitespace.core
  (:require [clj-whitespace.parser :as parser]
            [clj-whitespace.stackmachine :as stack-machine]
            [clojure.string :as string]
            [clojure.tools.cli :refer [parse-opts]]
            [instaparse.core :as insta]
            [clojure.pprint])
  (:gen-class))

  (def cli-options
    [["-i" "--intermediate" "Generate intermediate Clojure for the Whitespace source" :default false]
     ["-h" "--help" :default false]])
  
  (defn usage [options-summary]
  (->> ["This program executes Whitespace source, with the option of producing an intermediate representation."
        ""
        "Usage: clj-whitespace [options] source-file"
        ""
        "Options:"
        options-summary
        ""]
        (string/join \newline)))
  
  (defn error-msg [errors]
    (str "The following errors occurred while parsing your command:\n\n"
          (string/join \newline errors)))
  
  (defn validate-args [args]
    "We validate arguments for functionality in the main method."
    (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
      (cond
        (:help options) {:exit-message (usage summary) :ok? true}
        errors {:exit-message (error-msg errors)}
        (and (= 1 (count arguments)) (:intermediate options))
          {:action :generate-intermediate :file (first arguments)}
        (and (= 1 (count arguments)) (not (:intermediate options)))
          {:action :execute-source :file (first arguments)}
        :else ; failed custom validation => exit with usage summary
        {:exit-message (usage summary)})))
  
  (defn -main [& args]
    "This function is the main class"
    (let [{:keys [action file exit-message ok?]} (validate-args args)]
      (if exit-message
        (do (println exit-message) 
            (System/exit (if ok? 0 1)))
        (case action
          :generate-intermediate (clojure.pprint/pprint(parser/parse (slurp file)))
          :execute-source (stack-machine/interpret (parser/parse (slurp file)))))))
