(defproject clj-whitespace "0.1.0-SNAPSHOT"
  :description "A Whitespace compiler in Clojure"
  :url "https://github.com/jacksyou/clj-whitespace"
  :license {:name "MIT Public License"
            :url "https://opensource.org/licenses/MIT"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot clj-whitespace.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
