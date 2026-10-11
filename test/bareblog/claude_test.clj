(ns bareblog.claude-test
  "Holds what CLAUDE.md says of the code to the code."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(defn- namespace-of
  "The namespace of the source file at `path` under `src`."
  [path]
  (-> (str (fs/strip-ext (fs/relativize "src" path)))
      (str/replace fs/file-separator ".")
      (str/replace "_" "-")))

(defn- read-namespaces
  "The namespaces of the source files under `src`."
  []
  (set (map namespace-of (fs/glob "src" "**.{clj,cljc,cljs}"))))

(defn- read-listed
  "The namespaces that the table of CLAUDE.md lists."
  []
  (set (map second (re-seq #"(?m)^    (bareblog[\w.-]*)\s" (slurp "CLAUDE.md")))))

(deftest the-table-of-namespaces-lists-every-source-file-and-no-other
  (is (= (read-namespaces) (read-listed))))
