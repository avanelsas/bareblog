(ns bareblog.example
  "A directory to change in a test: one with nothing in it, or a copy of the example blog,
   with its settings, its posts, its drafts and its pages. And what a command says there."
  (:require [babashka.fs :as fs]
            [bareblog.blog.pictures :as pictures]
            [clojure.edn :as edn]))

(def dir "example")

(def git-settings
  "The settings of a git repository that a test commits in: who commits, and no housekeeping of
   git in a process of its own."
  {"user.name"        "A test"
   "user.email"       "test@example.com"
   "commit.gpgsign"   "false"
   "gc.auto"          "0"
   "maintenance.auto" "false"})

(defn- little-end [n]
  [(bit-and n 0xff) (bit-shift-right n 8)])

(defn write-gif!
  "Writes the start of a GIF of `width` by `height` pixels, which is all that is read of its
   size, as the file `file-name` in the directory `target`."
  [target file-name width height]
  (fs/write-bytes (fs/path target file-name)
                  (byte-array (map unchecked-byte (concat (map int "GIF89a") (little-end width) (little-end height))))))

(defn- copy-to!
  "Copies what an author wrote of the example blog into the directory `target`, with
   `settings` put over its settings, and the pictures beside them."
  [target settings]
  (doseq [posts ["posts" "drafts" "pages"]]
    (fs/copy-tree (fs/path dir posts) (fs/path target posts)))
  (let [own (edn/read-string (slurp (fs/file dir "blog.edn")))]
    (doseq [src (distinct (map :src (pictures/named own)))]
      (fs/copy (fs/path dir src) (fs/path target src)))
    (spit (fs/file target "blog.edn") (pr-str (merge own settings)))))

;; A repository that a test made may still be changed by git, in a process of its own that keeps
;; its house after a commit or a push. A file then comes or goes while the directory is deleted.
;; The tests that commit turn that off, so this holds for a repository that does not.
(defn- delete-dir!
  "Deletes the directory `dir` and what it holds, trying again for a while when what it holds
   changes as it is deleted."
  [dir]
  (loop [tries-left 30]
    (when-not (try
                (fs/delete-tree dir)
                true
                (catch java.io.IOException problem
                  (if (pos? tries-left) false (throw problem))))
      (Thread/sleep 100)
      (recur (dec tries-left)))))

(defn with-copy
  "What `check` gives for the directory of a copy of the example blog, with `settings` put
   over its settings. The copy is gone afterwards."
  [settings check]
  (let [target (fs/create-temp-dir)]
    (try
      (copy-to! target settings)
      (check (str target))
      (finally
        (delete-dir! target)))))

(defn run-command
  "What `command` says and ends with, run for `where` and `args` with `answer` to its question."
  [command where answer & args]
  (let [said (atom [])
        exit (apply command {:say (partial swap! said conj) :ask (constantly answer)} where args)]
    {:exit exit :said @said}))

(defn with-empty-dir
  "What `check` gives for a directory with nothing in it. It is gone afterwards."
  [check]
  (let [dir (str (fs/create-temp-dir))]
    (try
      (check dir)
      (finally
        (delete-dir! dir)))))
