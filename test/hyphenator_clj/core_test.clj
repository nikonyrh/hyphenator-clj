(ns hyphenator-clj.core-test
  (:require [clojure.test :refer :all]
            [hyphenator-clj.core :refer :all]))

(defmacro my-are     [fun & args]           `(are [inp exp] (= (~fun inp) exp) ~@args))
(defmacro my-deftest [test-name fun & args] `(deftest ~test-name (my-are ~fun ~@args)))

(my-deftest test-is-digit is-digit? \a false \_ false \Z false \0 true \5 true \9 true)
(my-deftest test-elem-max elem-max [[-3 1 3] [1 2 -3]] [1 2 3])

(deftest test-count-chars (are [char-str word-str exp]
                            (= (count-chars char-str word-str) exp)
                            "<>"     "a <img/><"   {\< 2 \> 1}
                            "abcd"   "def"         {\a 0 \b 0 \c 0 \d 1}))

(my-deftest test-hyphenate-word (partial hyphenate-word \-) "algorithm" "al-go-rithm" "test" "test")
(my-deftest test-hyphenate hyphenate
  "!Text hyphenation"                                 "!Text hy-phen-ation"
  "begins by splitting!"                              "be-gins by split-ting!"
  "not &hyphenated; samples"                          "not &hyphenated; sam-ples"
  "<hyphenated hyphenated style='hyphenated'>hyphenated<hyphenated/>"
  "<hyphenated hyphenated style='hyphenated'>hy-phen-ated<hyphenated/>") 

; (run-tests)
