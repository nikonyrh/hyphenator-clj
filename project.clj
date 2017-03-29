(defproject org.clojars.nikonyrh.hyphenator-clj "0.1.1"
  :description "Text hyphenation algorithm"
  :url         "https://github.com/nikonyrh/hyphenator-clj"
  :license {:name "Apache License, Version 2.0"
            :url  "http://www.apache.org/licenses/LICENSE-2.0"}
  :scm {:name "git"
        :url  "https://github.com/nikonyrh/hyphenator-clj"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :javac-options ["-target" "1.6" "-source" "1.6" "-Xlint:-options"]
  :aot [hyphenator-clj.core]
  :main hyphenator-clj.core)
