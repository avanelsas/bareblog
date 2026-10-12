(ns bareblog.cli
  "The `bareblog` command: what its arguments ask for, and the doing of it.

   The edge of the command. It is run in the directory of a blog, and is told where BareBlog
   is."
  (:require [babashka.fs :as fs]
            [bareblog.home :as home]
            [bareblog.source :as source]
            [bareblog.start :as start]
            [clojure.java.browse :as browse]
            [clojure.string :as str]))

(def usage
  (str/join "\n"
            ["bareblog init \"My blog\"           a new blog, in this directory"
             "bareblog new \"A new blog post\"    a draft folder with post.md"
             "bareblog preview [port]           the whole blog, in the browser"
             "                 --no-open        without opening the browser"
             "bareblog publish a-new-blog-post  one post: new, edited or removed"
             "bareblog publish --all            every post, as it is on disk"
             "                 --yes            with a yes to every question"]))

(def default-port 8080)

(def ^:private last-port 65535)

;; What the arguments ask for. Pure.

(defn- asks-title
  "What reads the title that a command takes, and refuses with `how` when there is none."
  [how]
  (fn [[title & more]]
    (if (and (not (str/blank? title)) (empty? more))
      {:title title}
      {:refuse how})))

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

(def ^:private all "--all")

(def ^:private yes "--yes")

(defn- asks-publish [args]
  (let [[what & more] (remove #{yes} args)
        yes?          (boolean (some #{yes} args))]
    (cond
      (and (= all what) (empty? more))                    {:all? true :yes? yes?}
      (and what (empty? more) (not= \- (first what)))     {:folder what :yes? yes?}
      :else {:refuse "Give the folder of a post, or --all: bareblog publish a-new-blog-post"})))

(defn- asks-nothing [_args]
  {})

;; The doing. Each function below gives the number that the process ends with.

(defn- run-init! [{:keys [dir]} {:keys [title]}]
  (let [{:keys [said problem]} (start/blog! dir title)]
    (if problem
      (do (println problem) 1)
      (do (println (str "A blog in " dir ". " said))
          (println)
          (println start/next-steps)
          0))))

(defn- run-new! [{:keys [dir]} {:keys [title]}]
  (let [{:keys [file problem]} (start/draft! dir title)]
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

(defn- ask!
  "Puts `question` to the author, and gives true for a yes."
  [question]
  (print (str question " [y/N] "))
  (flush)
  (contains? #{"y" "yes"} (str/lower-case (str/trim (or (read-line) "")))))

(defn- say-yes!
  "Says `question` and that the answer is yes, so that what a yes to every question let
   through is on record. Gives true."
  [question]
  (println (str question " yes"))
  true)

;; What publishes is asked for by name, as the server is.
(defn- run-publish! [{:keys [dir home]} {:keys [yes?] :as asked}]
  (let [{:keys [refusal] :as ready} ((requiring-resolve 'bareblog.generate/ready) home)
        publish!                    (requiring-resolve 'bareblog.publish/publish!)
        done?                       (requiring-resolve 'bareblog.publish/done?)]
    (if refusal
      (do (println refusal) 1)
      (let [how  (assoc ready :say println :ask (if yes? say-yes! ask!))
            came (publish! how dir (assoc asked :today (str (java.time.LocalDate/now))))]
        (if (done? (:outcome came)) 0 1)))))

(defn- run-help! [_where _asked]
  (println usage)
  0)

(def ^:private commands
  "Each command: what reads its arguments, what does it, and whether it is only done in the
   directory of a blog."
  {"init"    {:asks (asks-title "Give the title of the blog, in quotes: bareblog init \"My blog\"")
              :run  run-init!}
   "new"     {:asks       (asks-title "Give the title of the post, in quotes: bareblog new \"A new blog post\"")
              :run        run-new!
              :in-a-blog? true}
   "preview" {:asks asks-preview :run run-preview! :in-a-blog? true}
   "publish" {:asks asks-publish :run run-publish! :in-a-blog? true}
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
