(ns bareblog.command.cli
  "The `bareblog` command: what its arguments ask for, and the doing of it.

   The edge of the command. It is run in the directory of a blog, and is told where BareBlog
   is."
  (:require [babashka.fs :as fs]
            [bareblog.command.start :as start]
            [bareblog.read.source :as source]
            [clojure.java.browse :as browse]
            [clojure.string :as str]))

(def usage
  (str/join "\n"
            ["bareblog init \"My blog\"           a new blog, in this directory"
             "bareblog new \"A new blog post\"    a draft folder with post.md"
             "             --page \"About\"      a page that is no post, with page.md"
             "bareblog preview [port]           the whole blog, in the browser"
             "                 --no-open        without opening the browser"
             "bareblog publish a-new-blog-post  one post or page: new, edited or removed"
             "bareblog publish --all            every post and page, as it is on disk"
             "                 --yes            with a yes to every question"
             "bareblog tags                     every tag, and each written in more than one way"
             "bareblog tags rename old \"new\"    a tag, in every post and page"
             "              --yes               with a yes to the question"
             "bareblog series                   every series, with its parts in order"
             "bareblog series --name \"A series\" \"A post\" \"Another post\""
             "                                  makes a series, or replaces it, of these"
             "                                  posts in this order, by title or folder"
             "bareblog series --name \"A series\""
             "                                  removes the series, and keeps its posts"
             "                --yes             with a yes to the question"
             "bareblog version                  the version of BareBlog"
             "bareblog update                   BareBlog, brought to its latest release"
             "                --yes             with a yes to the question"]))

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

(def ^:private page "--page")

(defn- asks-new
  "What reads the title of a new post, or of a new page with the flag for one."
  [args]
  (assoc ((asks-title "Give the title, in quotes: bareblog new \"A new blog post\"") (remove #{page} args))
         :page? (boolean (some #{page} args))))

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
      :else {:refuse "Give the folder of a post or a page, or --all: bareblog publish a-new-blog-post"})))

(defn- asks-tags [args]
  (let [[what from to & more] (remove #{yes} args)
        yes?                  (boolean (some #{yes} args))]
    (cond
      (empty? args)                                    {}
      (and (= "rename" what) from to (empty? more))    {:rename [from to] :yes? yes?}
      :else {:refuse "Give a tag and its new name, in quotes: bareblog tags rename old \"new\""})))

(def ^:private name-flag "--name")

(defn- asks-series [args]
  (let [[what series-name & named] (remove #{yes} args)
        yes?                       (boolean (some #{yes} args))]
    (cond
      (empty? args)                             {}
      (and (= name-flag what) series-name)      {:series-name series-name :named (vec named) :yes? yes?}
      :else {:refuse "Name the series and its posts: bareblog series --name \"A series\" \"A post\" \"Another post\""})))

(defn- asks-nothing [_args]
  {})

(defn- asks-update [args]
  (if (every? #{yes} args)
    {:yes? (boolean (seq args))}
    {:refuse "Give nothing, or --yes: bareblog update"}))

;; The doing. Each function below gives the number that the process ends with.

(defn- run-init! [{:keys [dir]} {:keys [title]}]
  (let [{:keys [said problem]} (start/blog! dir title)]
    (if problem
      (do (println problem) 1)
      (do (println (str "A blog in " dir ". " said))
          (println)
          (println start/next-steps)
          0))))

(defn- run-new! [{:keys [dir]} {:keys [title page?]}]
  (let [{:keys [file problem]} ((if page? start/page! start/draft!) dir title)]
    (println (or problem (str "Write it in " file)))
    (if problem 1 0)))

;; The server is asked for by name, so that a command that serves nothing does not load it.
(defn- run-preview! [{:keys [dir home]} {:keys [port open?]}]
  (try
    (let [start!  (requiring-resolve 'bareblog.preview.serve/start!)
          app-dir (requiring-resolve 'bareblog.preview.serve/app-dir)]
      (start! (app-dir home) dir port))
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

;; A text gets the moment of its first publish as its date, on the clock of the author, so
;; that the day in it is the day the author published.
(defn- now []
  (.format (java.time.OffsetDateTime/now) (java.time.format.DateTimeFormatter/ofPattern "uuuu-MM-dd'T'HH:mm:ssxxx")))

;; What publishes is asked for by name, as the server is.
(defn- run-publish! [{:keys [dir home]} {:keys [yes?] :as asked}]
  (let [{:keys [refusal] :as ready} ((requiring-resolve 'bareblog.publish.generate/ready) home)
        publish!                    (requiring-resolve 'bareblog.publish.run/publish!)
        done?                       (requiring-resolve 'bareblog.publish.run/done?)]
    (if refusal
      (do (println refusal) 1)
      (let [how  (assoc ready :say println :ask (if yes? say-yes! ask!))
            came (publish! how dir (assoc asked :now (now)))]
        (if (done? (:outcome came)) 0 1)))))

;; The tags are changed by name, as a publish is.
(defn- run-tags! [{:keys [dir]} {:keys [rename yes?]}]
  (let [how {:say println :ask (if yes? say-yes! ask!)}]
    (if-some [[from to] rename]
      ((requiring-resolve 'bareblog.command.tags/rename!) how dir from to)
      ((requiring-resolve 'bareblog.command.tags/list!) how dir))))

;; The series are changed by name, as the tags are.
(defn- run-series! [{:keys [dir]} {:keys [series-name named yes?]}]
  (let [how {:say println :ask (if yes? say-yes! ask!)}]
    (if series-name
      ((requiring-resolve 'bareblog.command.series/make!) how dir series-name named)
      ((requiring-resolve 'bareblog.command.series/list!) how dir))))

;; The version and an update are asked for by name, as a publish is.
(defn- run-version! [{:keys [home]} _asked]
  ((requiring-resolve 'bareblog.command.update/version!) {:say println} home))

(defn- run-update! [{:keys [home]} {:keys [yes?]}]
  ((requiring-resolve 'bareblog.command.update/update!) {:say println :ask (if yes? say-yes! ask!)} home))

(defn- run-help! [_where _asked]
  (println usage)
  0)

(def ^:private commands
  "Each command: what reads its arguments, what does it, and whether it is only done in the
   directory of a blog."
  {"init"    {:asks (asks-title "Give the title of the blog, in quotes: bareblog init \"My blog\"")
              :run  run-init!}
   "new"     {:asks asks-new :run run-new! :in-a-blog? true}
   "preview" {:asks asks-preview :run run-preview! :in-a-blog? true}
   "publish" {:asks asks-publish :run run-publish! :in-a-blog? true}
   "tags"    {:asks asks-tags :run run-tags! :in-a-blog? true}
   "series"  {:asks asks-series :run run-series! :in-a-blog? true}
   "version" {:asks asks-nothing :run run-version!}
   "update"  {:asks asks-update :run run-update!}
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
