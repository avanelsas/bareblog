(ns bareblog.publish.git
  "What a blog asks of git, in its directory: a repository, a commit and a push.

   The edge of the command. Every call names the directory, so that nothing is done in another
   repository."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [clojure.string :as str]))

(defn- git
  "What git says to `args` in the directory `dir`: its `:exit`, its `:out` and its `:err`."
  [dir & args]
  (apply process/shell {:out :string :err :string :continue true} "git" "-C" (str dir) args))

(defn- ok? [result]
  (zero? (:exit result)))

(defn- why
  "The line of `text` in which git says what went wrong, or else its last line."
  [text]
  (let [lines (remove str/blank? (str/split-lines (str text)))]
    (or (first (filter (partial re-find #"^(fatal|error):") lines)) (last lines))))

;; A blog is the top of a repository of its own. A directory inside another repository, such
;; as the example blog inside BareBlog, is not one, and nothing is committed for it.
(defn top?
  "True when the directory `dir` is the top of a git repository."
  [dir]
  (let [said (git dir "rev-parse" "--show-toplevel")]
    (and (ok? said)
         (= (str (fs/canonicalize dir)) (str (fs/canonicalize (str/trim (:out said))))))))

(defn inside?
  "True when the directory `dir` is in a git repository, at its top or below it."
  [dir]
  (ok? (git dir "rev-parse" "--show-toplevel")))

(def branch
  "The branch that a new blog starts on. GitHub Pages serves it."
  "main")

(defn init!
  "Makes the directory `dir` a git repository. Gives a `:problem` when git could not."
  [dir]
  (let [said (git dir "init" "--quiet" "--initial-branch" branch)]
    (if (ok? said)
      {}
      {:problem (or (why (:err said)) "git could not make a repository.")})))

(defn- known?
  "True when the path `path` in `dir` is there, or is one that git keeps."
  [dir path]
  (or (fs/exists? (fs/path dir path))
      (ok? (git dir "ls-files" "--error-unmatch" "--" path))))

(defn commit!
  "Commits what changed under the paths `paths` of the repository in `dir`, and nothing else,
   with the message `message`. Gives `:committed?`, which is false when nothing changed, or a
   `:problem`."
  [dir paths message]
  (let [paths  (filter (partial known? dir) paths)
        staged (apply git dir "add" "--all" "--" paths)]
    (cond
      (not (ok? staged))
      {:problem (why (:err staged))}

      (ok? (apply git dir "diff" "--cached" "--quiet" "--" paths))
      {:committed? false}

      :else
      (let [said (apply git dir "commit" "--quiet" "-m" message "--" paths)]
        (if (ok? said)
          {:committed? true}
          {:problem (or (why (:err said)) (why (:out said)))})))))

(defn push!
  "Pushes the branch of the repository in `dir` to where it is pushed. Gives a `:problem` when
   git could not."
  [dir]
  (let [said (git dir "push" "--quiet")]
    (if (ok? said)
      {}
      {:problem (or (why (:err said)) "git could not push.")})))
