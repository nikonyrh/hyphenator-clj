(ns hyphenator-clj.core
  (:require [clojure.string  :as str]
            [clojure.set     :as set]
            [clojure.java.io :as io])
  (:gen-class))

(let [zero (int \0) nine (int \9)] (defn is-digit? [c] (<= zero (int c) nine)))

(defn read-file "Read a file with optional line-specific parser"
  ([fname]        (read-file nil fname))
  ([parser fname] (with-open [rdr (io/reader fname)]
                    (->> rdr line-seq (map (or parser identity)) doall))))

(defn elem-max "Element-wise maximum of two or more vectors"
  ([vecs]      (reduce elem-max vecs))
  ([vec1 vec2] (mapv max vec1 vec2)))

(defn get-word "Prefix and postfix the word with underscore, also allocate :values for record keeping"
  [w] {:str (str "_" w "_") :values (vec (concat [10] (repeat (count w) 0) [10]))})
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Gradually parses a file line like "_gen3t4" into {:str "_gent", :digits {4 3, 5 4}}
(let [zero      (int \0)
      to-digit  (fn [idx digit] {(- (:idx digit) idx) (- (int (:char digit)) zero)})
      to-groups (fn [line] (->> line
                             (map-indexed (fn [idx chr] {:idx idx :char chr}))
                             (group-by #(if (is-digit? (:char %)) :digits :chars))))
      to-strs   (fn [line] (let [groups (to-groups line)]
                             {:str    (apply str   (map :char (:chars groups)))
                              :digits (apply merge (map-indexed to-digit (:digits groups)))}))]
  (def patterns (->> "english.txt" (read-file to-strs) (filter (partial :digits)))))

(defn match-pattern [word pattern]
  "Given a word an a pattern, checks on which indexes the pattern is found (if any), returns values vec or nil"
  (let [word-str      (str/lower-case (:str word))
        pattern-str   (:str pattern)
        pattern-len   (count pattern-str)
        create-values (fn [values idx] (reduce #(assoc %1 (+ idx -1 (first %2)) (last %2)) values (seq (:digits pattern))))]
    (loop [this-idx 0 result (:values word)]
       (let [idx-of (.indexOf word-str pattern-str this-idx)]
         (if (neg? idx-of) (if (some pos? result) result nil)
           (recur (+ idx-of pattern-len) (elem-max result (create-values result idx-of))))))))

(defn hyphenate-word [hyphen word-str]
  "Given a word, iterate over all patterns and accumulate the maximum vec value.
   Then add hyphens where the value is odd and strings are long enough."
  (let [match-patterns (fn [word] (->> patterns
                                    (map (partial match-pattern word))
                                    (filter some?)
                                    (reduce elem-max (:values word))))
        matches        (-> word-str get-word match-patterns)
        long-enough?   (partial <= 2)]
    (loop [result [] chunk-len 0 m matches w word-str]
      (if (empty? w)
        (apply str result)
        (let [w-rest     (rest w)
              add-hyphen (and (odd? (first m)) (long-enough? chunk-len) (long-enough? (count w-rest)))]
          (recur
            (conj (if add-hyphen (conj result hyphen) result) (first w))
            (if add-hyphen 1 (inc chunk-len))
            (rest m)
            w-rest))))))

;(map (partial hyphenate-word \-)      ["algorithm" "example"])
;(map (partial hyphenate-word "&shy;") ["algorithm" "example"])

; Find which characters (upper and lowercase) occur in sequences, excluding the underscore
(let [lower-chars (->> patterns (map (comp seq :str)) flatten (remove #{\_}) set)
      upper-str   (->> lower-chars (apply str) str/upper-case)]
  (def pattern-chars (set (concat lower-chars upper-str))))


(defn count-chars [char-str word-str]
  "For each character in char-str, return the number of occurances in word-str"
  (let [freq (frequencies word-str)] (->> (for [char char-str] [char (get freq char 0)]) (into {}))))


(defn hyphenate [sentence & {:keys [hyphen] :or {hyphen \-}}]
  "Given a sentence, partition it into words hand hyphenate them individually"
  (let [hyphenator        (partial hyphenate-word hyphen)
        join              (partial str/join "")
       ; Partition into chunks of "word letters" (a-z A-Z) and the rest
        words             (->> sentence (partition-by (partial contains? pattern-chars)) (mapv join))
        ; We must check whether words are at odd or even indexes
        word-idx?         (if (contains? pattern-chars (first sentence)) even? odd?)
        ; The difference between "open" and "close" HTML markings in a word
        tag-balance       (fn [counts] (- (counts \<) (counts \>)))
        ; The accumulated number of open and closed brackets
        tag-balances      (->> words (map #(->> % (count-chars "<>") tag-balance)) (reductions +) vec)
        ; No open HTML tags, we are at a word index, not in &escaped;
        should-hyphenate? (fn [word-idx] (and (= 0 (tag-balances word-idx))
                                              (word-idx? word-idx)
                                              (not (and (> word-idx 0)
                                                        (< word-idx (count words))
                                                        (= \& (last  (words (dec word-idx))))
                                                        (= \; (first (words (inc word-idx))))))))]
    (->> words (map-indexed #(if (should-hyphenate? %1) (hyphenate-word hyphen %2) %2)) join)))

;(hyphenate "Text hyphenation begins by splitting the string into chunks using sequences of non-alphabetical characters as delimiters")
;(hyphenate "<hyphenated hyphenated style='hyphenated'>hyphenated &hyphenated;<hyphenated/>")
;(hyphenate "hyphenation" :hyphen "&shy;")

(defn -main [& argv] (doall (for [arg argv]
                              (println (hyphenate arg :hyphen \-)))))
