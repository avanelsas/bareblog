(ns bareblog.source
  "Reads a blog from its directory: its settings, its posts, and the data of each page.

   The edge of the command, for both the preview and the generator. A blog is read as a map
   says: its directory under `:dir`, the top of its addresses under `:root`, and under
   `:preview?` whether it is read for its author, with its drafts, or for its readers.
   `preview` and `published` give the two maps there are."
  (:require [babashka.fs :as fs]
            [bareblog.address :as address]
            [bareblog.app.templates :as templates]
            [bareblog.image :as image]
            [bareblog.layout :as layout]
            [bareblog.output :as output]
            [bareblog.post :as post]
            [bareblog.reader :as reader]
            [bareblog.site :as site]
            [clojure.edn :as edn]
            [clojure.java.io :as io]))

(defn- shows-newest?
  "True when the blog with the settings `settings` shows its newest post on its home page."
  [settings]
  (= :newest-post (:home (templates/of-blog settings))))

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
  {:dir dir :root (:root (address/of-blog (:address settings))) :preview? false})

(defn- post-dirs
  "The directories of `blog` that hold a folder for each post that is served."
  [{:keys [preview?]}]
  (if preview? layout/post-dirs [layout/posts-dir]))

(defn find-post-dir
  "The directory of the post `folder` in `blog`, or nil when there is none."
  [{:keys [dir] :as blog} folder]
  (first (filter fs/directory? (map #(fs/path dir % folder) (post-dirs blog)))))

(defn read-settings
  "The settings of the blog in the directory `dir`. Throws with the name of the file when it
   cannot be read."
  [dir]
  (try
    (edn/read-string (slurp (fs/file dir layout/settings-file)))
    (catch Exception problem
      (throw (ex-info (str layout/settings-file ": " (.getMessage problem)) {} problem)))))

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
  (when-some [size (read-size (fs/path dir (post/file-beside source)))]
    [source size]))

(defn- read-post
  "The post in the directory `dir`, with the size of each image beside it."
  [folder dir]
  (let [post (post/from folder (reader/read-post (slurp (fs/file dir layout/post-file))))]
    (post/sized post (into {} (keep (partial read-sized dir)) (post/sources post)))))

(defn- list-dirs
  "Every directory in the post directories of `blog`, with its name, its place from the top of
   the blog, and whether it is a draft."
  [{:keys [dir] :as blog}]
  (for [post-dir (post-dirs blog)
        path     (when (fs/directory? (fs/path dir post-dir)) (sort (fs/list-dir (fs/path dir post-dir))))
        :when    (fs/directory? path)]
    {:folder (fs/file-name path) :dir path :place (str post-dir "/" (fs/file-name path))
     :draft? (= post-dir layout/drafts-dir)}))

(defn- post-dir? [{:keys [folder]}]
  (post/folder? folder))

(defn- refused
  "What is wrong with a directory whose name is no name of a post folder."
  [{:keys [place]}]
  (str place " is left out. " post/folder-rule))

(defn- read-entry
  "The entry of the post in a directory, and what is wrong with the post."
  [{:keys [dir draft? place] folder-name :folder}]
  (let [post (read-post folder-name dir)]
    {:entry    (site/entry folder-name post draft?)
     :problems (map #(str place ": " %) (:problems post))}))

(defn read-site
  "What every page of `blog` loads, with what is wrong with the blog."
  [{:keys [dir] :as blog}]
  (let [dirs (list-dirs blog)
        read (map read-entry (filter post-dir? dirs))
        site (site/site (read-settings dir)
                        (map :entry read)
                        (concat (map refused (remove post-dir? dirs)) (mapcat :problems read)))]
    (site/with-problems site (output/problems site))))

(defn- read-shown-post
  "The post in the folder `folder-name` as a page shows it: with the name of its folder, and
   with each file beside it under its full address, so that any page can show it. Nil when
   there is no such post."
  [{:keys [root] :as blog} folder-name]
  (when-some [dir (some->> folder-name (find-post-dir blog))]
    (-> (read-post folder-name dir)
        (post/placed (address/post root folder-name))
        (assoc :folder folder-name))))

(defn- read-post-page [blog {folder-name :folder :as page} _site]
  (when-some [post (read-shown-post blog folder-name)]
    {:page page :post post}))

;; The settings say what the home page shows, and they are one small file. The list of posts
;; is read only when the home page shows a post, and nobody read it yet.
(defn- read-home-page
  "The data of a page of the home page. It holds the newest post when the blog template shows
   that on its home page."
  [blog page site]
  (let [post (when (shows-newest? (or (:blog site) (read-settings (:dir blog))))
               (read-shown-post blog (site/newest-folder (or site (read-site blog)))))]
    (cond-> {:page page} post (assoc :post post))))

(defn- read-plain-page [_blog page _site]
  {:page page})

(def ^:private page-readers
  "What reads the data of each kind of page."
  {:post read-post-page
   :home read-home-page})

(defn read-page
  "The data of the page `page` of `blog`: what the page is, and the post that it shows when it
   shows one. Nil when it is the page of a post that is not there. With `site`, the site as it
   was read, nothing of it is read again."
  ([blog page]
   (read-page blog page nil))
  ([blog page site]
   ((get page-readers (:kind page) read-plain-page) blog page site)))
