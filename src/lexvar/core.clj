(ns lexvar.counter
  (:require [my-utils.io :refer :all]
            [my-utils.syntax :refer [invert-map map-vk]]
            [bindnlp.mate-tools-clj :as mate]
            [bindnlp.tt4j-clj :as tt4j]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.core.matrix :refer [new-sparse-array mset!]]))

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Semantic SPACE
;;;;;;;;;;;;;;;;;;;;;;;;

(defn compute-space
  "docs is an iterable of already preprocessed sents, context-value-fn 
  determine which word belongs to the context and weights it"
  ([docs context-value-fn] (compute-space docs context-value-fn false))
  ([docs context-value-fn targets]
   (let [space (atom {})]
     (doseq [doc docs
             target (or targets doc)
             :let [contexts (context-value-fn target doc)]]
       (doseq [context contexts
               [w v] context]
         (if (get-in @space [target w])
           (swap! space update-in [target w] (partial + v))
           (swap! space assoc-in  [target w] v))))
     @space)))

(defn n-window 
  "for each match returns a seq of contexts and their weights" 
  [target s n]
  (let [chunks (partition (inc n) 1 s)
        result (concat
                (map rest      (filter #(= target (first %)) chunks))
                (map drop-last (filter #(= target (last  %)) chunks)))]
    (map #(map vector % (repeat 1)) result)))

(defn sum-col [context-word space]
  (reduce + (for [context (vals space)
                  :when (context-word context)]
              (context-word context))))

(defn sum-row [target-word space]
  (reduce + (vals (target-word space))))

(defn index-rows
  "returns a map of row words (targets) to idx"
  [space]
  (zipmap (keys space) (range)))

(defn index-cols
  "returns a map of col words (contexts) to idx"
  [space]
  (let [cols (atom {})
        i (atom 0)]
    (doseq [[target contexts] space
            context (keys contexts)
            :when (not (contains? @cols context))]
      (swap! cols assoc-in [context] @i)
      (swap! i inc))
    @cols))

(defn index-space
  "return a tuple of the index-rows and index-cols
  at once"
  [space]
  (let [rows (zipmap (keys space) (range))
        cols (atom {})
        i (atom 0)]
    (doseq [[target contexts] space context (keys contexts)
            :when (not (contains? @cols context))]
      (swap! assoc-in cols context @i)
      (swap! i inc))
    [rows @cols]))

(defn to-sparse  
  [space rows cols]
  (let [M (new-sparse-array :vectorz (count rows) (count cols))]
    (doseq [[target contexts] space
            [context v] contexts]
      (mset! M (target rows) (context cols) v))
    M))

;;; output functions
(defn- space->sspace-sparse
  "https://code.google.com/p/airhead-research/wiki/FileFormats#Input_Document_File_Formats"
  [space index-row index-col out-fn]
  (with-open [wrt (io/writer (str out-fn ".sspace"))]
    (binding [*out* wrt]
      (println (str (count index-row) " " (count index-col)))
      (doseq [[target context] space
              :let [context-str (apply str (interpose " " (flatten (map (fn [[w v]] [(get index-col w) v]) context))))]]
        (println (str target " | " context-str))))))

(defn- load-s-space-sparse
  [space-fn cols]
  (let [space (atom {})
        lines (rest (lazy-lines space-fn))
        inv-cols (invert-map (frm-load cols))]
    (doseq [line lines
            :let [[key val] (map s/trim (s/split line #"\|"))
                  contexts (map parse-number (s/split val #" "))
                  contexts-map (map-vk (apply hash-map contexts)
                                       #(get inv-cols %))]]
      (swap! space assoc key contexts-map))))

(defn- space->dissect-sparse
  "http://clic.cimec.unitn.it/composes/toolkit/matrix_file.html#cooccurrence-matrix-file"
  [space index-row index-col out-fn]
  (with-open [wrt (io/writer (str out-fn ".sm"))]
    (binding [*out* wrt]
      (doseq [[target context] space
              [word value] context]
        (println (str target "\t" word "\t" value)))))
  (with-open [wrt (io/writer (str out-fn ".rows"))]
    (binding [*out* wrt]
      (doseq [w (keys index-row)] (println w))))
  (with-open [wrt (io/writer (str out-fn ".cols"))]
    (binding [*out* wrt]
      (doseq [w (keys index-col)] (println w)))))

(defn load-dissect-sparse
  [space-fn]
  {:pre [(.endsWith space-fn ".sm")]}
  (let [lines (lazy-lines space-fn)
        space (atom {})]
    (doseq [line lines
            :let [[row col val] (s/split line #"[ \t]")]]
      (swap! space assoc-in [row col] (parse-number val)))
    @space))

(defn save-space
  "Unpure function. It writes a space to a seq of specifed formats.
  Defaults to a s-expression format. pre-indexed rows and cols can be 
  provided. Otherwise indexes will be computed and only stored if
  specified in the format vector"
  [space out-fn & {:keys [format index-row index-col] :or {format [:default]}}]
  {:pre [(vector? format)]}
  (if (and (= :default (first format)) (= 1 (count format)))
    (frm-save (str out-fn ".ss.clj") space)
    (let [index-row (or index-row (index-rows space))
          index-col (or index-col (index-cols space))]
      (doseq [f format]
        (case f
          :row (frm-save (str out-fn ".rows.clj") index-row)
          :col (frm-save (str out-fn ".cols.clj") index-col)
          :s-space-sparse (space->sspace-sparse space index-row index-col out-fn)
          :dissect-sparse (space->dissect-sparse space index-row index-col out-fn)
          :default        (frm-save (str out-fn ".ss.clj") space))))))

(defn load-space
  [space-fn & {:keys [format cols-fn]}]
  {:pre [(if (= format :s-space-sparse) cols-fn true)]}
  (case format
    :s-space-sparse (load-s-space-sparse space-fn cols-fn)
    :dissect-sparse (load-dissect-sparse space-fn)
    :default (frm-load space-fn)))

;;; tests
(def test-file "/Users/quique/code/clojure/lexvar/resources/test1.sents")
(def test-dir "/Users/quique/code/clojure/lexvar/resources")
(def sents (lazy-lines test-file))
(def sents-dir (lazy-lines test-dir :input :dir))

(defn process-sent [s]
  (map #(.toLowerCase %)
       (-> s
           s/trim
           (s/split #" "))))

(def m {":a" {":A" 1 ":B" 2 ":R" 6} 
        ":b" {":A" 2 ":V" 3 ":H" 1}
        ":c" {":D" 2 ":R" 3 ":V" 1}})

(space->file m "mmap" :format [:s-space-sparse :row :col])
(= m (load-space "mmap.sspace" "mmaprows.clj" "mmapcols.clj" :format :s-space-sparse))

(space->file m "mmap" :format [:dissect-sparse])
(= m (load-space "mmap.sm" :format :dissect-sparse))

(def tagger (mate/create-tagger "/Users/quique/code/clojure/bindnlp/models/ger-mate-tools/tag-ger-3.6.model"))



