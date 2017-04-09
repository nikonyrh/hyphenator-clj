# Hyphenator-clj

A re-implementation of a hyphenation algorithm I originally did in [PHP](https://github.com/nikonyrh/hyphenator-php/). Available at [Clojars](https://clojars.org/org.clojars.nikonyrh.hyphenator-clj):

    [org.clojars.nikonyrh.hyphenator-clj "0.1.1"]

Usage:

    (ns example.core (:require [hyphenator-clj.core :as h]))
    (defn -main [& argv] (doseq [arg argv] (println (h/hyphenate arg :hyphen \-))))
