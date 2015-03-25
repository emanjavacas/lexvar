(ns lexvar.counter
  (:require [my-utils.io :refer [frm-save frm-load lazy-lines]]
            [clojure.java.io :as io]
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

(defn space->file
  "Unpure function. It writes a space to a specifed format.
  Defaults to a s-expression format"
  [space out-fn & {:keys [format index-row index-col]
                   :or {format :default}}]
  (case format
    :row (frm-save out-fn (or index-row (index-rows space)))
    :col (frm-save out-fn (or index-col (index-cols space)))
    :s-space-sparse (space->sspace-sparse
                     space
                     (or index-col (index-cols space))
                     (or index-row (index-rows space))
                     out-fn)
    :dissect-sparse (space->dissect-sparse
                     (or index-col (index-cols space))
                     (or index-row (index-rows space))                     
                     space out-fn)
    :default (frm-save out-fn space)))

;;; tests
(def test-sent ["toen" "ik" "jong" "was" "." "toen"])
(n-window "toen" test-sent 3)

(def m {:a {:A 1 :B 2 :R 6} 
        :b {:A 2 :V 3 :H 1}
        :c {:D 2 :R 3 :V 1}})

;; (space->sspace-sparse m (index-rows m) (index-cols m) "test")
;; (index-cols m)
;; (sum-col :V m)
;; (sum-row :a m)

(def test-file "/Users/quique/code/clojure/lexvar/resources/test1.sents")
(def test-dir "/Users/quique/code/clojure/lexvar/resources")
(def sents (lazy-lines test-file))
(count sents)
(def sents-dir (lazy-lines test-dir :input :dir))
(count sents-dir)

(defn process-sent [s]
  (map #(.toLowerCase %)
       (-> s
           clojure.string/trim
           (clojure.string/split #" "))))

(process-sent (first sents))

(def my-sents (map process-sent sents-dir))
(def space (compute-space my-sents (fn [w sent] (n-window w sent 4))))
(space->file space "300words" :format :s-space-sparse)
(space->file m (io/file "dummy1") :format :row)
(space->file m "test.test" :format :row)

