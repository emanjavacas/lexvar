(ns lexvar.io
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [my-utils.io :refer :all]
            [my-utils.syntax :refer :all]))

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

;;; output functions
(defn- space->sspace-sparse
  "https://code.google.com/p/airhead-research/wiki/FileFormats#Input_Document_File_Formats"
  [space index-row index-col out-fn]
  (with-open [wrt (io/writer (str out-fn ".sspace"))]
    (binding [*out* wrt]
      (println (str (count index-row) " " (count index-col)))
      (doseq [[target context] space
              :let [context-str
                    (apply str (interpose " " (flatten (map (fn [[w v]] [(get index-col w) v]) context))))]]
        (println (str target "|" context-str))))))

(defn- load-s-space-sparse
  [space-fn cols]
  (let [space (atom {})
        lines (rest (lazy-lines space-fn))
        inv-cols (invert-map (frm-load cols))]
    (doseq [line lines
            :let [[key val] (map s/trim (s/split line #"\|"))
                  contexts (map parse-number (s/split val #" "))
                  contexts-map (map-vk (apply hash-map contexts) #(get inv-cols %))]]
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

(defn- next-chunk [coll]
  (loop [curr coll acc []]
    (when-let [item (first curr)]
      (cond (re-matches #"</s>" item) acc
            (re-matches #"<s>" item) (recur (rest curr) acc)                     
            :else (recur (rest curr) (conj acc item))))))

(defn- parse-vrt
  "parse a vrt file to sents given sent sep <s>,</s>"
  [lines]
  (lazy-seq
   (when-let [s (seq lines)]
     (let [chunk (next-chunk s)]
       (cons chunk (parse-vrt (nthrest s (+ 2 (count chunk)))))))))

(defn parse-corpus
  "a seq of lines, if format is word per line
  input is parsed to seq of sents with the help
  of sent separators (parse-vrt)."
  [lines & {:keys [input output]}]
  (letfn [(->str [ids]
            (for [[token pos lemma] (map #(s/split % #"\t") ids)]
              (str lemma "_" pos)))
          (->map [ids]
            (map #(zipmap [:pos :lemma :token] %) (map #(s/split % #"\t") ids)))]
    (let [output-fn (if (= output :map) ->map ->str)]
      (cond (= input :sent) (map output-fn (map #(s/split % #"\t") lines))
            (= input :word) (map output-fn (parse-vrt lines))))))
