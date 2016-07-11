(ns hyphenator-clj.core (:gen-class))

(require '[clojure.string  :as str])
(require '[clojure.set     :as set])
(require '[clojure.java.io :as io])

(let [zero (int \0) nine (int \9)] (defn is-digit? [c] (<= zero (int c) nine)))

(defn read-file "Read a file with optional line-specific parser"
  ([fname] (read-file fname nil))
  ([fname parser] (with-open [rdr (io/reader fname)]
                    (let [lines (line-seq rdr)] (if (some? parser)
                                                  (doall (map parser lines))
                                                  (doall lines))))))

(defn elem-max "Element-wise maximum of two or more vectors"
  ([vecs] (reduce elem-max vecs))
  ([vec1 vec2] (vec (map #(max %1 %2) vec1 vec2))))

(defn get-word "Prefix and postfix the word with underscore, also allocate :values for record keeping"
  [w] {:str (str "_" w "_") :values (vec (concat [10] (repeat (count w) 0) [10]))})
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Gradually parses a file line like "_gen3t4" into {:str "_gent", :digits {4 3, 5 4}}
(let [zero      (int \0)
      to-digit  (fn [idx digit] {(- (:idx digit) idx) (- (int (:char digit)) zero)})
      to-groups (fn [line] (group-by #(if (is-digit? (:char %)) :digits :chars)
                             (map-indexed (fn [idx chr] {:idx idx :char chr}) line)))
      to-strs   (fn [line] (let [groups (to-groups line)]
                             {:str    (apply str (map :char (:chars groups)))
                              :digits (apply merge (map-indexed to-digit (:digits groups)))}))]
  (def patterns (filter (partial :digits) (read-file "english.txt" to-strs))))

(defn match-pattern [word pattern]
  "Given a word an a pattern, checks on which indexes the pattern is found (if any), returns values vec or nil"
  (let [word-str      (str/lower-case (:str word))
        pattern-str   (:str pattern)
        pattern-len   (count pattern-str)
        create-values (fn [values idx] (reduce #(assoc %1 (+ idx -1 (first %2)) (last %2)) values (seq (:digits pattern))))]
    ((fn [this-idx result]
        (let [idx-of (.indexOf word-str pattern-str this-idx)]
          (if (neg? idx-of) (if (some pos? result) result nil)
            (recur (+ idx-of pattern-len) (elem-max result (create-values result idx-of))))))
     0 (:values word))))

(defn hyphenate-word [hyphen word-str]
  "Given a word, iterate over all patterns and accumulate the maximum vec value.
   Then add hyphens where the value is odd and strings are long enough."
  (let [match-patterns (fn [word] (reduce elem-max (:values word)
                                    (filter some? (map (partial match-pattern word) patterns))))
        matches        (match-patterns (get-word word-str))
        long-enough?   (partial <= 2)]
    ((fn [result chunk-len m w]
      (if (empty? w)
        (apply str result)
        (let [w-rest     (rest w)
              add-hyphen (and (odd? (first m)) (long-enough? chunk-len) (long-enough? (count w-rest)))]
          (recur
            (conj (if add-hyphen (conj result hyphen) result) (first w))
            (if add-hyphen 1 (inc chunk-len))
            (rest m)
            w-rest))))
     [] 0 matches word-str)))

;(map (partial hyphenate-word \-)      ["algorithm" "example"])
;(map (partial hyphenate-word "&shy;") ["algorithm" "example"])

; Find which characters (upper and lowercase) occur in sequences, excluding the underscore
(let [lower-chars (-> (into #{} (flatten (map #(seq (:str %)) patterns))) (disj \_))
      upper-str   (str/upper-case (apply str lower-chars))]
  (def pattern-chars (set/union lower-chars (into #{} upper-str))))

(defn hyphenate [sentence & {:keys [hyphen] :or {hyphen \-}}]
  "Given a sentence, partition it into words hand hyphenate them individually"
  (let [hyphenator        (partial hyphenate-word hyphen)
        join              (partial str/join "")
        first-contains    (contains? pattern-chars (first sentence)) ; We must check whether words are at odd or even indexes
        words             (map join (partition-by (partial contains? pattern-chars) sentence))
        should-hyphenate? #(= (even? %) first-contains)]
    (join (map-indexed #(if (should-hyphenate? %1) (hyphenator %2) %2) words))))
;(hyphenate "Text hyphenation begins by splitting the string into chunks using sequences of non-alphabetical characters as delimiters")
;(hyphenate "hyphenation" :hyphen "&shy;")

(defn -main [& argv] (doall (map #(println (hyphenate % :hyphen \-)) argv)))
