(ns lexvar.counter
  (:require [lexvar.io :refer :all]
            [my-utils.syntax :refer :all]
            [my-utils.io :refer :all]
            [clj-progress.core :refer :all]
            [clojure.string :as s]
            [clojure.tools.cli :refer [parse-opts]])
  (:gen-class))

(set! *warn-on-reflection* true)
;;;;;;;;;;;;;;;;;;;;;;;;
;;; Semantic SPACE
;;;;;;;;;;;;;;;;;;;;;;;;
(defn sliding-window
  [target s n & {:keys [partial?]}]
  (let [fx (if partial? (fn [^String w ^String prf] (.startsWith w prf)) =)]
    (loop [todo s seen [] acc []]
      (cond (empty? todo) acc
            (fx (first todo) target)
            (recur (rest todo)
                   (cons (first todo) seen)
                   (concat acc (take n seen) (take n (rest todo))))
            :else (recur (rest todo) (cons (first todo) seen) acc)))))

(defn compute-context-map [docs context-fn target]
  (->> (mapcat #(context-fn target %) docs) ; get the context words for a given target
       (group-by identity)
       (map (fn [[k v]] [k (count v)]))
       (into {})))

(defn compute-space
  ([docs context-fn targets]
   (into {} (map #(vector % (compute-context-map docs context-fn %)) targets))))

(defn pcompute-space
  [docs step context-fn targets]
  (init (count (partition-all step targets)))
  (let [result (into {} (pmap (fn [chunk]
                                (do (tick))
                                (compute-space docs context-fn chunk))
                              (partition-all step targets)))]
    (done)
    result))

(defn chunks [lines]
  (for [line lines w line] w))

(defn pfrequencies
  "http://lilyx.net/2011/06/11/calculating-n-gram-statistics-in-a-mapreduce-way-using-clojure/"
  [coll step f]
  (reduce
   #(merge-with + %1 %2)
   (pmap (fn [chunk]
           (frequencies (f chunk)))
         (partition-all step coll))))

(def cli-options
  [["-i" "--input INPUT" "Input dir"]
   ["-o" "--output OUTPUT" "Output file"]
   ["-t" "--targets TARGETS" "File with targets in clj format"]
   ["-s" "--step STEP" "Length of the chunks at parallelizing (measure in sents)" :parse-fn #(parse-number %)]
   ["-n" "--window-size WINDOW-SIZE" "Arg to sliding-window" :parse-fn #(parse-number %)]
   ["-p" "--partial PARTIAL" "Partial matching on targets (startsWith routine instead of equality)" :default false]])

(defn -main [& args]
    (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)
          input-format (if (.isDirectory (java.io.File. (:input options))) :dir :file)
          sents (parse-corpus (lazy-lines (:input options) :input input-format) :input :word)
          ts (lazy-lines (:targets options))
          context-fn (fn [target sent] (sliding-window target sent (:window-size options) :partial? (:partial options)))
          space (pcompute-space sents (:step options) context-fn ts)]
      (println (str "Saving space to " (:output options)))
      (save-space space (:output options) :format [:dissect-sparse])))
