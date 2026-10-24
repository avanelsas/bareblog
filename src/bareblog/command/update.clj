(ns bareblog.command.update
  "The `bareblog version` and `bareblog update` commands: the version of BareBlog, and BareBlog
   brought to its latest release.

   The edge of the command. BareBlog is a clone in `home`, and a release is a tag `vX.Y.Z`."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [cheshire.core :as json]
            [clojure.string :as str]))

;; What a version is, and which release is newer. Pure.

(defn version
  "The version that `text` names, as `0.3.0` or `v0.3.0`, as three numbers. Nil when it names
   none."
  [text]
  (some->> (re-matches #"v?(\d+)\.(\d+)\.(\d+)" (str text)) rest (mapv parse-long)))

(defn- shown
  "The version `v` as an author reads it: `0.3.0`."
  [v]
  (str/join "." v))

(defn- newer?
  "True when the version `a` comes after the version `b`."
  [a b]
  (pos? (compare a b)))

(defn- release
  "The version of the release that a tag such as `v0.3.0` marks, or nil."
  [tag-name]
  (when (str/starts-with? tag-name "v")
    (version tag-name)))

(defn latest
  "The newest version among the tags `tag-names`. Nil when none of them is a release."
  [tag-names]
  (last (sort (keep release tag-names))))

(defn- section-version
  "The version of the section of a changelog that starts with `section`, or nil."
  [section]
  (some-> (re-find #"^## \[([^\]]+)\]" section) second version))

(defn- between?
  "True when `section` of a changelog is of a version after `from`, up to and with `to`."
  [from to section]
  (when-some [v (section-version section)]
    (and (newer? v from) (not (newer? v to)))))

(defn changes
  "The sections of the changelog `changelog` of the versions after `from`, up to and with `to`,
   as one text. Empty when there are none."
  [changelog from to]
  (->> (str/split changelog #"(?m)^(?=## \[)")
       (filter (partial between? from to))
       (map str/trim)
       (str/join "\n\n")))

(defn refusal
  "Why BareBlog in `home` may not be moved to another release, from what git says of it, or nil
   when it may. It may when it is a clone with no change of its own, on the branch `main`, or on
   no branch at a release."
  [home {:keys [clone? changed? branch release?]}]
  (cond
    (not clone?)
    (str "BareBlog in " home " is not a clone of its repository. Update it as you installed it.")

    changed?
    (str "BareBlog in " home " has changes of its own. Commit or remove them, and update again.")

    (not (if branch (= "main" branch) release?))
    (str "BareBlog in " home " is on " (or branch "a commit that is no release")
         ". Switch to main, and update again.")))

(defn plan
  "What an update does for BareBlog in `home` at the version `from`, from what git says of it
   and the tags `tag-names` of its repository, which are nil when it could not be reached: why
   it is `:refused`, that it is `:unreachable?`, that `from` is `:current`, or the release `:to`."
  [home from state tag-names]
  (let [refused (refusal home state)
        to      (latest tag-names)]
    (cond
      refused                   {:refused refused}
      (nil? tag-names)          {:unreachable? true}
      (and to (newer? to from)) {:to to}
      :else                     {:current from})))

;; Doing it.

(defn- run
  "What `args` give when run in the directory `dir`: its `:exit`, its `:out` and its `:err`."
  [dir & args]
  (apply process/shell {:dir (str dir) :out :string :err :string :continue true} args))

(defn- ok? [result]
  (zero? (:exit result)))

(defn- git
  "What git says to `args` in BareBlog in `home`, as text, trimmed. Nil when git could not."
  [home & args]
  (let [said (apply run home "git" args)]
    (when (ok? said)
      (str/trim (:out said)))))

(defn- read-version
  "The version of BareBlog in `home`, as its `package.json` names it."
  [home]
  (version (get (json/parse-string (slurp (fs/file home "package.json"))) "version")))

(defn- read-state
  "What git says of BareBlog in `home`: whether it is a clone, whether it has changes of its
   own, the branch it is on, and whether it is on a release."
  [home]
  (let [top (git home "rev-parse" "--show-toplevel")]
    {:clone?   (and (some? top) (= (str (fs/canonicalize home)) (str (fs/canonicalize top))))
     :changed? (not (str/blank? (git home "status" "--porcelain" "--untracked-files=no")))
     :branch   (not-empty (git home "symbolic-ref" "--short" "--quiet" "HEAD"))
     :release? (some? (latest (str/split-lines (or (git home "tag" "--points-at" "HEAD") ""))))}))

(defn- read-tags
  "The tags of BareBlog in `home`, after it got those of its repository. Nil when it could not
   get them."
  [home]
  (when (git home "fetch" "--tags" "--quiet" "origin")
    (str/split-lines (or (git home "tag" "--list" "v*") ""))))

(defn version!
  "Says the version of BareBlog in `home`."
  [{:keys [say]} home]
  (say (str "BareBlog " (shown (read-version home))))
  0)

(defn- steps
  "What brings BareBlog to the release `to`, in order: what each step is called, and what it
   runs."
  [to]
  [{:called (str "Getting " (shown to)) :runs ["git" "checkout" "--quiet" (str "v" (shown to))]}
   {:called "Installing what it needs"  :runs ["npm" "ci"]}
   {:called "Building the app"          :runs ["npx" "shadow-cljs" "release" "app"]}])

(defn- last-lines
  "The last lines of `text`, at most `n`, as one text."
  [n text]
  (str/join "\n" (take-last n (remove str/blank? (str/split-lines (str text))))))

(defn- step!
  "Runs the step `called` in BareBlog in `home`, and says what went wrong when it fails. Gives
   true when it ran."
  [{:keys [say]} home {:keys [called runs]}]
  (say (str called " ..."))
  (let [said (apply run home runs)]
    (or (ok? said)
        (do (say (last-lines 20 (str (:out said) "\n" (:err said))))
            (say (str "That failed. Run it in " home " yourself: " (str/join " " runs)))
            false))))

(defn- moved!
  "Moves BareBlog in `home` from the version `from` to the release `to`, once the author says
   yes to what changes."
  [{:keys [say ask] :as how} home from to]
  (say (str "BareBlog " (shown from) " is installed. The latest release is " (shown to) "."))
  (say "")
  (say (changes (or (git home "show" (str "v" (shown to) ":CHANGELOG.md")) "") from to))
  (say "")
  (cond
    (not (ask (str "Update BareBlog to " (shown to) "?")))
    (do (say "Nothing was changed.") 0)

    (every? (partial step! how home) (steps to))
    (do (say (str "BareBlog is " (shown to) ". Run bareblog publish --all in each blog to put it live."))
        0)

    :else 1))

(defn update!
  "Brings BareBlog in `home` to its latest release, once the author says yes to what changes."
  [{:keys [say] :as how} home]
  (let [from                              (read-version home)
        {:keys [refused unreachable? to]} (plan home from (read-state home) (read-tags home))]
    (cond
      refused      (do (say refused) 1)
      unreachable? (do (say "The repository of BareBlog could not be reached. Nothing was changed.") 1)
      to           (moved! how home from to)
      :else        (do (say (str "BareBlog " (shown from) " is the latest release.")) 0))))
