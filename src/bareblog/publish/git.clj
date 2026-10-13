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

;; Git says what went wrong on a line that starts with `fatal:` or `error:`, or else on lines
;; of their own, and may follow them with hints, which say what to do with git and nothing of
;; what went wrong.
(defn- why
  "What git says went wrong in `text`: the line that says so, or else every line that is no
   hint, as one. Nil when it says nothing."
  [text]
  (let [lines (remove str/blank? (str/split-lines (str text)))]
    (or (first (filter (partial re-find #"^(fatal|error):") lines))
        (not-empty (str/join " " (remove (partial re-find #"^hint:") lines))))))

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

;; Git says of each path which line of which file decides about it, as `.gitignore:3:docs/`,
;; a tab, and the path. A line that starts with `!` lets the path in again, so a path that
;; such a line decides about is not kept out. Git is asked without regard to what it holds
;; already, since it refuses to add a path that a line keeps out also when it holds files
;; below that path.
(defn- entry
  "What the line `line` of git says of a path that is kept out: the `:path`, the `:pattern`
   that keeps it out, and the `:source` that the pattern is in. Nil for a path that is let
   in. `asked` gives each path by the way git was asked about it."
  [asked line]
  (when-some [[_ source pattern path] (re-matches #"(.*):\d+:(.*)\t(.*)" line)]
    (when-not (str/starts-with? pattern "!")
      {:path (asked path path) :pattern pattern :source source})))

(defn- ask-kept-out
  "An entry for each path that git keeps out among the keys of `asked`, in the repository in
   `dir`."
  [dir asked]
  (let [said (apply git dir "-c" "core.quotePath=false" "check-ignore" "--no-index" "--verbose"
                    "--" (keys asked))]
    (if (ok? said)
      (into [] (keep (partial entry asked)) (str/split-lines (:out said)))
      [])))

;; A line such as `docs/` keeps out a directory and no file. Git takes a path that is not
;; there for a file, so a path is asked for as a directory unless it is a file.
(defn- as-asked
  "The path `path` in `dir` as git is asked about it: as a directory, unless it is a file."
  [dir path]
  (if (fs/regular-file? (fs/path dir path))
    path
    (str path "/")))

(defn list-ignored
  "What the repository in `dir` keeps out of git among the paths `paths`, which a commit of
   them would refuse: each `:path`, with the `:pattern` and the `:source` that keep it out. A
   path that is not there yet is taken for a directory."
  [dir paths]
  (ask-kept-out dir (zipmap (map (partial as-asked dir) paths) paths)))

;; A file below a path that is committed may be kept out by a line of its own, as `*.jpg`
;; keeps out a photo. Git commits the path without it and says nothing.
(defn list-ignored-files
  "The files below the paths `paths` of the repository in `dir` that git keeps out, and would
   leave out of a commit of those paths, each as `list-ignored` gives it."
  [dir paths]
  (let [said  (apply git dir "-c" "core.quotePath=false" "ls-files" "--others" "--ignored"
                     "--exclude-standard" "--" paths)
        files (remove str/blank? (str/split-lines (:out said)))]
    (if (and (ok? said) (seq files))
      (ask-kept-out dir (zipmap files files))
      [])))

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
