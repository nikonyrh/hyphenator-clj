(defproject hyphenator-clj "0.0.1-SNAPSHOT"
  :description "Text hyphenation algorithm"
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :javac-options ["-target" "1.6" "-source" "1.6" "-Xlint:-options"]
  :aot [hyphenator-clj.core]
  :main hyphenator-clj.core)
