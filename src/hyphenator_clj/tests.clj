(ns hyphenator-clj.core (:gen-class) (:use clojure.test))

(defmacro my-are     [fun & args]           `(are [inp exp] (= (~fun inp) exp) ~@args))
(defmacro my-deftest [test-name fun & args] `(deftest ~test-name (my-are ~fun ~@args)))

(my-deftest test-is-digit is-digit? \a false \_ false \Z false \0 true \5 true \9 true)
(my-deftest test-elem-max elem-max [[-3 1 3] [1 2 -3]] [1 2 3])

(my-deftest test-hyphenate-word (partial hyphenate-word \-) "algorithm" "al-go-rithm" "test" "test")
(my-deftest test-hyphenate hyphenate "!Text hyphenation" "!Text hy-phen-ation" "begins by splitting!" "be-gins by split-ting!")

(run-tests)
