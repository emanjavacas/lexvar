(ns lexvar.mat-operations
  (:require [clojure.core.matrix :refer [new-sparse-array mset!]]))

(defn sum-col [context-word space]
  (reduce + (for [context (vals space)
                  :when (context-word context)]
              (context-word context))))

(defn sum-row [target-word space]
  (reduce + (vals (target-word space))))

(defn to-sparse  
  [space rows cols]
  (let [M (new-sparse-array :vectorz (count rows) (count cols))]
    (doseq [[target contexts] space
            [context v] contexts]
      (mset! M (target rows) (context cols) v))
    M))
