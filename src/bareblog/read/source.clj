(ns bareblog.read.source
  "Reads a blog from its directory: its settings, its posts, and the data of each page.

   The edge of the command, for both the preview and the generator. A blog is read as a map
   says: its directory under `:dir`, the top of its addresses under `:root`, and under
   `:preview?` whether it is read for its author, with its drafts, or for its readers.
   `preview` and `published` give the two maps there are.

   A blog that is read for its readers may say more. `:settings` are its settings, in place of
   the file. `:read` is the set of folders that are read, of posts and of pages, where it is
   not all of them. `:as-published` is the set of those that are taken as they were published
   last, from the output directory, and not read. `:dates` gives the date of each post and
   page, which is the moment it was first published."
  (:require [babashka.fs :as fs]
            [bareblog.blog.address :as address]
            [bareblog.blog.shape :as shape]
            [bareblog.blog.site :as site]
            [bareblog.blog.text :as text]
            [bareblog.blog.wire :as wire]
            [bareblog.blog.layout :as layout]
            [bareblog.read.image :as image]
            [bareblog.read.reader :as reader]
            [bareblog.written.output :as output]
            [clojure.edn :as edn]
            [clojure.java.io :as io]))

(defn- shows-newest?
  "True when the blog with the settings `settings` shows its newest post on its home page."
  [settings]
  (= :newest-post (:home (shape/of-blog settings))))

(defn blog?
  "True when the directory `dir` is the top of a blog."
  [dir]
  (and (some? dir) (fs/regular-file? (fs/path dir layout/settings-file))))

(defn refusal
  "Why the directory `dir` is no blog."
  [dir]
  (str "No " layout/settings-file " in " dir ". A blog has one at its top."))

(defn preview
  "How the blog in the directory `dir` is read for its author: at the top of its host, with
   its drafts."
  [dir]
  {:dir dir :root address/top :preview? true})

(defn published
  "How the blog in the directory `dir` is read for its readers, for the settings `settings`:
   at the address they name, without its drafts."
  [dir settings]
  {:dir dir :root (:root (address/of-blog (:address settings))) :preview? false :settings settings})

(defn- read-dirs
  "The directories of `blog` that hold a folder for each thing that is read: a post, a draft
   when the blog is read for its author, and a page that is no post. A post goes before a page
   of the same name."
  [{:keys [preview?]}]
  (if preview?
    layout/written-dirs
    [layout/posts-dir layout/pages-dir]))

(defn- find-source
  "Where the folder `folder` of `blog` is: its directory under `:dir`, and the directory that
   it is in under `:in`. Nil when there is none."
  [{:keys [dir] :as blog} folder]
  (first (for [in    (read-dirs blog)
               :let  [path (fs/path dir in folder)]
               :when (fs/directory? path)]
           {:folder folder :dir path :in in})))

(defn find-dir
  "The directory of the post or the page `folder` in `blog`, or nil when there is none."
  [blog folder]
  (:dir (find-source blog folder)))

(defn- standalone?
  "True when what is in the directory `in` is a page that is no post."
  [{:keys [in]}]
  (= layout/pages-dir in))

(defn read-settings
  "The settings of the blog in the directory `dir`. Throws with the name of the file when it
   cannot be read."
  [dir]
  (try
    (edn/read-string (slurp (fs/file dir layout/settings-file)))
    (catch Exception problem
      (throw (ex-info (str layout/settings-file ": " (.getMessage problem)) {} problem)))))

(defn- settings-of
  "The settings of `blog`: those it was given, or else those in its file."
  [{:keys [dir settings]}]
  (or settings (read-settings dir)))

(defn read-published
  "The site of the blog in the directory `dir` as it was published last, or nil when it was
   never published."
  [dir]
  (let [file (fs/file dir layout/output-dir wire/site-data)]
    (when (fs/regular-file? file)
      (edn/read-string (slurp file)))))

(defn- read-published-page
  "The data of the page of `folder` of the blog in the directory `dir` as it was published
   last, or nil."
  [dir folder]
  (let [file (fs/file dir layout/output-dir folder wire/page-data)]
    (when (fs/regular-file? file)
      (edn/read-string (slurp file)))))

(def ^:private head 262144)

(defn- read-head
  "The first bytes of the file at `path`, at most `head` of them."
  [path]
  (with-open [in (io/input-stream (fs/file path))]
    (.readNBytes in head)))

(defn- read-size
  "The size of the image at `path`, or nil when it has none. The start of the file is read,
   and all of it when the start does not hold the size."
  [path]
  (when (fs/regular-file? path)
    (or (image/size (read-head path))
        (when (< head (fs/size path))
          (image/size (fs/read-all-bytes path))))))

(defn- read-sized
  "The address `source` and the size of the image it names in `dir`, or nil when it has none."
  [dir source]
  (when-some [size (read-size (fs/path dir (text/file-beside source)))]
    [source size]))

;; The date of a post or a page is the moment it was first published, which the published
;; list keeps. A preview shows it too, so that a text has the same date wherever it is seen. A
;; preview also knows which pages are published, to say of the others that they are not.
(defn- with-published
  "The `blog` with what a preview takes from the published list, each where it was given
   none: the date of each post and page, and the folders of the published pages."
  [{:keys [dir preview? dates published-pages] :as blog}]
  (if (and preview? (or (nil? dates) (nil? published-pages)))
    (let [published (read-published dir)]
      (cond-> blog
        (nil? dates)           (assoc :dates (site/dates published))
        (nil? published-pages) (assoc :published-pages (into #{} (map :folder) (:pages published)))))
    blog))

(defn- read-text
  "The post or the page of `blog` in `source`, with the size of each image beside it, and with
   the date that the blog gives it, which is the moment it was first published."
  [{:keys [dates]} {:keys [folder dir in]}]
  (let [read  (text/from folder (reader/read-post (slurp (fs/file dir (layout/text-file in)))))
        sized (text/sized read (into {} (keep (partial read-sized dir)) (text/sources read)))]
    (cond-> sized
      (get dates folder) (assoc :date (get dates folder)))))

(defn- named-well? [{:keys [folder]}]
  (text/folder? folder))

;; A post and a page of the same name would have the same address. The post is the one that
;; is shown, and the author is told.
(defn- with-shadows
  "The directories `dirs`, each page with the name of a post marked as shadowed."
  [dirs]
  (let [posts (into #{} (comp (remove standalone?) (map :folder)) dirs)]
    (map (fn [source]
           (cond-> source
             (and (standalone? source) (contains? posts (:folder source))) (assoc :shadowed? true)))
         dirs)))

(defn- list-dirs
  "Every directory in the directories that `blog` reads, with its name, its place from the top
   of the blog, the directory that it is in, whether it is a draft, and whether it is a page
   with the name of a post."
  [{:keys [dir] :as blog}]
  (with-shadows
    (for [in    (read-dirs blog)
          path  (when (fs/directory? (fs/path dir in)) (sort (fs/list-dir (fs/path dir in))))
          :when (fs/directory? path)]
      {:folder (fs/file-name path) :dir path :in in :place (str in "/" (fs/file-name path))
       :draft? (= in layout/drafts-dir)})))

(def ^:private usable?
  "True when a directory can be read as a post or a page."
  (every-pred named-well? (complement :shadowed?)))

(defn list-folders
  "The folders of `blog` that can be read, as a set under `:posts` and one under `:pages`."
  [blog]
  (let [usable (filter usable? (list-dirs blog))]
    {:posts (into #{} (comp (remove standalone?) (map :folder)) usable)
     :pages (into #{} (comp (filter standalone?) (map :folder)) usable)}))

(defn- refused
  "What is wrong with a directory whose name is no name of the folder of a post or a page."
  [{:keys [place]}]
  (str place " is left out. " text/folder-rule))

(defn- shadowed-problem [{:keys [place folder]}]
  (str place " is left out. There is a post " folder " too, and both would have the same address."))

(defn- folder-problems
  "What is wrong with the directories `dirs` themselves, as texts."
  [dirs]
  (concat (map refused (remove named-well? dirs))
          (map shadowed-problem (filter :shadowed? dirs))))

(defn- unpublished
  "The entry `entry` of a page, marked as not published when `blog` is read for its author and
   the page was not published."
  [{:keys [preview? published-pages]} entry]
  (cond-> entry
    (and preview? (not (contains? published-pages (:folder entry)))) (assoc :unpublished? true)))

(defn- read-entry
  "The entry of the post or the page of `blog` in `source`, with whether it is a page."
  [blog {:keys [folder draft?] :as source}]
  (let [read (read-text blog source)]
    {:page? (standalone? source)
     :entry (if (standalone? source)
              (unpublished blog (site/page-entry folder read))
              (site/entry folder read draft?))}))

(defn- read?
  "True when `blog` reads `source`: it can be read, and is among the folders that the blog
   reads, where it does not read them all."
  [{:keys [read]} {:keys [folder] :as source}]
  (and (usable? source) (or (nil? read) (contains? read folder))))

(defn- read-entries
  "The entry of each post and each page that `blog` reads among the directories `dirs`."
  [blog dirs]
  (map (partial read-entry blog) (filter (partial read? blog) dirs)))

(defn- read-kept
  "The entries under `k`, `:posts` or `:pages`, that `blog` takes as they were published last."
  [{:keys [dir as-published]} k]
  (when (seq as-published)
    (filter (comp as-published :folder) (get (read-published dir) k))))

(defn read-problems
  "What is wrong with the posts that `blog` reads, as texts."
  [blog]
  (let [read (read-entries (with-published blog) (list-dirs blog))]
    (output/problems {:posts (map :entry (remove :page? read))})))

(defn read-site
  "What every page of `blog` loads, with what is wrong with the blog."
  [blog]
  (let [blog (with-published blog)
        dirs (list-dirs blog)
        read (read-entries blog dirs)
        site (-> (site/site (settings-of blog)
                            (concat (map :entry (remove :page? read)) (read-kept blog :posts))
                            (folder-problems dirs))
                 (site/with-pages (concat (map :entry (filter :page? read)) (read-kept blog :pages))))]
    (site/with-problems site (output/problems site))))

;; What is taken as it was published has its files from where they were published. The
;; output directory is still there while new pages are written beside it.
(defn find-media-dir
  "The directory that holds the files beside the post or the page in `folder` of `blog`."
  [{:keys [dir as-published] :as blog} folder]
  (if (contains? as-published folder)
    (fs/path dir layout/output-dir folder)
    (find-dir blog folder)))

(defn- shown
  "The text of `blog` in `source` as a page shows it: with the name of its folder, and with
   each file beside it under its full address, so that any page can show it."
  [{:keys [root] :as blog} {:keys [folder] :as source}]
  (-> (read-text (with-published blog) source)
      (text/placed (address/post root folder))
      (assoc :folder folder)))

(defn read-shown-text
  "The post or the page in the folder `folder` as a page shows it. One that `blog` takes as it
   was published is the one that was published. Nil when there is none."
  [{:keys [dir as-published] :as blog} folder]
  (if (contains? as-published folder)
    (:text (read-published-page dir folder))
    (some->> folder (find-source blog) (shown blog))))

;; The address of a post and of a page that is no post look the same, so a page is asked for
;; as a post. What is in the folder says which of the two it is.
(defn- read-text-page
  "The data of the page of the post or the page in the folder of `page`: what the page is,
   with its kind, and its text."
  [{:keys [dir as-published] :as blog} {folder :folder :as page} _site]
  (if (contains? as-published folder)
    (read-published-page dir folder)
    (when-some [source (find-source blog folder)]
      {:page (assoc page :kind (if (standalone? source) :standalone :post))
       :text (shown blog source)})))

;; The settings say what the home page shows, and they are one small file. The list of posts
;; is read only when the home page shows a post, and nobody read it yet.
(defn- read-home-page
  "The data of a page of the home page. It holds the newest post when the blog template shows
   that on its home page."
  [blog page site]
  (let [text (when (shows-newest? (or (:blog site) (settings-of blog)))
               (read-shown-text blog (site/newest-folder (or site (read-site blog)))))]
    (cond-> {:page page} text (assoc :text text))))

(defn- read-plain-page [_blog page _site]
  {:page page})

(def ^:private page-readers
  "What reads the data of each kind of page."
  {:post       read-text-page
   :standalone read-text-page
   :home       read-home-page})

(defn read-page
  "The data of the page `page` of `blog`: what the page is, and under `:text` the post or the
   page of the author that it shows, when it shows one. Nil when it is the page of a folder
   that is not there. With `site`, the site as it was read, nothing of it is read again."
  ([blog page]
   (read-page blog page nil))
  ([blog page site]
   ((get page-readers (:kind page) read-plain-page) blog page site)))
