(ns clj-whitespace.core
  (:require [clj-whitespace.parser :as parser]
            [clj-whitespace.interpreter :as interpreter]
            [clojure.string :as string]
            [clojure.tools.cli :refer [parse-opts]]
            [instaparse.core :as insta])
  (:gen-class))


