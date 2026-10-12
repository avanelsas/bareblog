(ns bareblog.cli
  "The `bareblog` command: what its arguments ask for, and the doing of it.

   The edge of the command. It is run in the directory of a blog, and is told where BareBlog
   is."
  (:require [babashka.fs :as fs]
            [bareblog.home :as home]
            [bareblog.layout :as layout]
            [bareblog.post :as post]
            [bareblog.source :as source]
            [clojure.java.browse :as browse]
            [clojure.string :as str]))

(def usage
  (str/join "\n"
            ["bareblog new \"A new blog post\"    a draft folder with post.md"
             "bareblog preview [port]           the whole blog, in the browser"
             "                 --no-open        without opening the browser"]))

(def default-port 8080)

(def ^:private last-port 65535)

;; What the arguments ask for. Pure.

(defn- asks-new [[title & more]]
  (if (and (not (str/blank? title)) (empty? more))
    {:title title}
    {:refuse "Give the title of the post, in quotes: bareblog new \"A new blog post\""}))

(def ^:private no-open "--no-open")

(defn- port?
  "True when `text` is the number of a port."
  [text]
  (and (some? (re-matches #"[1-9]\d{0,4}" text)) (<= (parse-long text) last-port)))

(defn- asks-preview [args]
  (let [[port & more] (remove #{no-open} args)]
    (if (and (empty? more) (or (nil? port) (port? port)))
      {:port  (if port (parse-long port) default-port)
       :open? (not-any? #{no-open} args)}
      {:refuse "Give a port, or nothing: bareblog preview [port] [--no-open]"})))

(defn- asks-nothing [_args]
  {})

(defn draft
  "The draft that `bareblog new` makes for the title `title`: its `:folder` and the `:text` of
   its post. A title that gives no name for a folder gives a `:problem`."
  [title]
  (let [folder (post/folder-for title)]
    (if (post/folder? folder)
      {:folder folder :text (str "# " (str/trim title) "\n\n")}
      {:problem (str "The title " (pr-str title) " gives no name for a folder. " post/folder-rule)})))

;; The doing. Each function below reads or writes the disk, and gives the number that the
;; process ends with.

(defn- write-draft!
  "Writes the post `text` of a draft in `folder`, in the blog in the directory `dir`."
  [dir folder text]
  (let [file (fs/path dir layout/drafts-dir folder layout/post-file)]
    (fs/create-dirs (fs/parent file))
    (spit (fs/file file) text)
    {:file (str file)}))

(defn new-draft!
  "Makes the draft for `title` in the blog in the directory `dir`. Gives the file of its post,
   or a `:problem` when the title gives no folder or the folder is there already."
  [dir title]
  (let [{:keys [folder text problem]} (draft title)]
    (if problem
      {:problem problem}
      (if-some [taken (source/find-post-dir (source/preview dir) folder)]
        {:problem (str "There is a post " folder " already, in " taken ".")}
        (write-draft! dir folder text)))))

(defn- run-new! [{:keys [dir]} {:keys [title]}]
  (let [{:keys [file problem]} (new-draft! dir title)]
    (println (or problem (str "Write the post in " file)))
    (if problem 1 0)))

;; The server is asked for by name, so that a command that serves nothing does not load it.
(defn- run-preview! [{:keys [dir home]} {:keys [port open?]}]
  (try
    ((requiring-resolve 'bareblog.serve/start!) (home/app-dir home) dir port)
    (when open?
      (browse/browse-url (str "http://localhost:" port "/")))
    @(promise)
    (catch java.net.BindException _
      (println (str "The port " port " is taken. Give another: bareblog preview " (inc port)))
      1)))

(defn- run-help! [_where _asked]
  (println usage)
  0)

(def ^:private commands
  "Each command: what reads its arguments, what does it, and whether it is only done in the
   directory of a blog."
  {"new"     {:asks asks-new :run run-new! :in-a-blog? true}
   "preview" {:asks asks-preview :run run-preview! :in-a-blog? true}
   "help"    {:asks asks-nothing :run run-help!}})

(defn asked
  "What the arguments `args` ask for: the `:command`, with what its arguments say. Arguments
   that ask for nothing known give what to `:refuse` them with."
  [[command & args]]
  (let [command (or command "help")]
    (if-some [{:keys [asks]} (commands command)]
      (assoc (asks args) :command command)
      {:refuse (str "There is no command " command ".")})))

(defn- refuse!
  "Says `text` and how the command is used."
  [text]
  (println text)
  (println)
  (println usage)
  1)

(defn main
  "Does what the arguments `args` ask for, in the directory that the command is run in, for
   BareBlog in the directory `home`. Gives the number that the process ends with."
  [home args]
  (let [dir                         (str (fs/cwd))
        {:keys [command refuse] :as asked} (asked args)
        {:keys [run in-a-blog?]}    (commands command)]
    (cond
      refuse                                   (refuse! refuse)
      (and in-a-blog? (not (source/blog? dir))) (refuse! (source/refusal dir))
      :else                                    (run {:dir dir :home home} asked))))
