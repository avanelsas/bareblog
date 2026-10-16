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

(def folders
  "The folders of the code. A folder uses those before it, and none after it."
  ["blog" "app" "written" "read" "preview" "publish" "command"])

(defn- read-folders
  "The folders of the code in the order that the table of CLAUDE.md gives them."
  []
  (mapv second (re-seq #"(?m)^    (\w+)/ " (slurp "CLAUDE.md"))))

(defn- folder-of
  "The folder of the namespace `named`, which is the part of its name after the first."
  [named]
  (second (str/split named #"\.")))

;; A docstring or a comment may name any namespace. Only what the code names is a use. A
;; string, a comment and a character are each taken out where they start, so that a quote or
;; a semicolon inside one of them starts nothing.
(defn- code
  "The text `text` of a source file without its strings, its comments and its characters."
  [text]
  (str/replace text #"\"(?:[^\"\\]|\\.)*\"|;[^\n]*|\\." ""))

(defn- read-required
  "The namespaces of BareBlog that the code of the source file at `path` names, without its
   own."
  [path]
  (disj (set (re-seq #"bareblog\.\w+\.[\w-]+" (code (slurp (str path))))) (namespace-of path)))

(defn- read-uses
  "Every use of a folder by a file in another folder, as the folder that uses and the folder
   that is used."
  []
  (distinct
   (for [path     (fs/glob "src" "**.{clj,cljc,cljs}")
         required (read-required path)
         :let     [user (folder-of (namespace-of path)) used (folder-of required)]
         :when    (not= user used)]
     [user used])))

(deftest the-folders-are-those-of-the-code-and-of-the-table-in-that-order
  (is (= (set folders) (set (map folder-of (read-namespaces)))))
  (is (= folders (read-folders))))

(deftest a-folder-uses-those-before-it-and-none-after
  (let [place (zipmap folders (range))]
    (doseq [[user used] (read-uses)]
      (is (< (place used) (place user)) (str user " uses " used)))))

;; The app is what a reader loads, and the command is what an author runs. What a blog is, is
;; all that they share.
(deftest the-app-and-the-command-share-what-a-blog-is-and-nothing-else
  (doseq [[user used] (read-uses)]
    (when (= "app" user)
      (is (= "blog" used) "the app uses nothing of the command but what a blog is"))
    (is (not= "app" used) (str user " uses the app"))))

(deftest a-file-is-required-under-the-last-part-of-its-name
  (doseq [path (concat (fs/glob "src" "**.{clj,cljc,cljs}") (fs/glob "test" "**.{clj,cljc}"))
          [_ required alias] (re-seq #"\[(bareblog\.[\w.-]+) :as ([\w-]+)\]" (slurp (str path)))]
    (is (= (last (str/split required #"\.")) alias) (str path))))

;; The tests, the app, the saver and a live frame each take BareDOM from a place of their own.
;; They are the same release.
(def ^:private version-places
  "Where BareDOM's version is named, and what finds it there."
  {"bb.edn"                     #"com\.github\.avanelsas/baredom \{:mvn/version \"([^\"]+)\"\}"
   "shadow-cljs.edn"            #"\[com\.github\.avanelsas/baredom \"([^\"]+)\"\]"
   "package.json"               #"\"@vanelsas/baredom\": \"([^\"]+)\""
   "src/bareblog/app/live.cljc" #"@vanelsas/baredom@([^/]+)/dist/"})

(defn- read-versions
  "BareDOM's version in each place that names it, by the name of the file."
  []
  (into {} (map (fn [[file pattern]] [file (second (re-find pattern (slurp file)))])) version-places))

(deftest every-place-names-the-same-release-of-baredom
  (let [versions (read-versions)]
    (is (every? some? (vals versions)) (str "A place names no version: " versions))
    (is (= 1 (count (set (vals versions)))) (str "The places name different versions: " versions))
    (is (every? #(re-matches #"\d+\.\d+\.\d+" (str %)) (vals versions))
        (str "A place names a range, not one version: " versions))))
