(defproject clj-whitespace "0.1.0-SNAPSHOT"
  :description "A Whitespace compiler in Clojure"
  :url "https://github.com/jacksyou/clj-whitespace"
  :license {:name "MIT Public License"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.match "0.3.0-alpha5"]
                 [org.clojure/tools.cli "0.3.5"]
                 ]
  :main ^:skip-aot clj-whitespace.core
  :java-source-paths ["src/" "test/"]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
