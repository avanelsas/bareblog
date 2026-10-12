(ns bareblog.serve
  "A local server for one blog: the app, and each post as data."
  (:require [babashka.fs :as fs]
            [bareblog.address :as address]
            [bareblog.app.templates :as templates]
            [bareblog.image :as image]
            [bareblog.layout :as layout]
            [bareblog.live :as live]
            [bareblog.post :as post]
            [bareblog.reader :as reader]
            [bareblog.site :as site]
            [bareblog.wire :as wire]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [org.httpkit.server :as http]))

(def default-port 8080)

(def public-dir
  "Where the app is, from the directory the server is started in."
  (fs/path "resources" "public"))


;; What a path asks for. Pure.

(def ^:private folder
  (str "(" post/folder-pattern ")"))

(def ^:private file
  (str "(" post/file-pattern ")"))

(defn- escaped
  "The text `text` as the text of a pattern that matches it and nothing else."
  [text]
  (str/escape text {\. "\\."}))

(defn- home-page [[number]]
  {:kind :home :number (if number (parse-long number) 1)})

(defn- tag-page
  "The page of the tag that an address spells as `spelled`, or nil when its escapes are broken."
  [[spelled]]
  (when-some [tag-name (post/decoded spelled)]
    {:kind :tag :tag tag-name}))

(defn- archive-page [_found]
  {:kind :archive})

(defn- post-page [[folder-name]]
  {:kind :post :folder folder-name})

(def ^:private pages
  "The pattern of the address of each kind of page, without its last slash, and what makes the
   page of what the pattern finds. They are tried in order."
  [[""                                      home-page]
   [(str "/" address/pages "/([1-9]\\d*)")   home-page]
   [(str "/" address/tags "/([^/]+)")        tag-page]
   [(str "/" address/archive-name)           archive-page]
   [(str "/" folder)                         post-page]])

(def ^:private beside-a-page
  "What each address beside a page serves: the page itself, its data, and its socket."
  [["/"                                :page]
   [(str "/" (escaped wire/page-data)) :data]
   [(str "/" wire/changes)             :changes]])

(def ^:private page-table
  "A pattern for the address of every kind of page and of everything beside one, with what it
   serves and what makes the page."
  (vec (for [[page-pattern page-of] pages
             [ending serve]         beside-a-page]
         [(re-pattern (str page-pattern ending)) serve page-of])))

(defn- found-by
  "What the groups of `pattern` find in `path`, in a vector. Nil when the pattern does not match."
  [pattern path]
  (when-some [matched (re-matches pattern path)]
    (if (string? matched) [] (subvec matched 1))))

(defn- page-route
  "What `path` serves by one row of the page table, or nil."
  [path [pattern serve page-of]]
  (when-some [page (some-> (found-by pattern path) page-of)]
    {:serve serve :page page}))

(defn- page-routes
  "What `path` serves when it is the address of a page, or of something beside one."
  [path]
  (some (partial page-route path) page-table))

(defn- redirect-route
  "A page address that `path` is but for its last slash, as a redirect to it."
  [path]
  (when (page-routes (str path "/"))
    {:serve :redirect :to (str path "/")}))

(def ^:private static-pattern #"/(js/[\w./-]+|[\w-]+\.\w+)")

(def ^:private media-pattern
  (re-pattern (str "/" folder "/" file)))

(defn- media-route
  "The file beside a post that `path` names, or nil."
  [path]
  (when-some [[folder-name spelled] (found-by media-pattern path)]
    (when-some [file-name (post/file-beside spelled)]
      {:serve :media :folder folder-name :file file-name})))

(defn- file-route
  "What `path` serves when it is a file: of the site, of the app, or beside a post."
  [path]
  (or (when (= path address/site-data)
        {:serve :site})
      (when-some [[static] (found-by static-pattern path)]
        {:serve :static :file static})
      (media-route path)))

;; A path is matched as it is written, and what a pattern finds is read after that. A slash
;; that is spelled as an escape is then part of a name, and no slash of the path.
(defn route
  "What the server serves for the path `path` of a request, as a map with `:serve`. A path that
   names nothing, or that holds two dots in a row, serves `:nothing`."
  [path]
  (or (when-not (str/includes? path "..")
        (or (page-routes path) (redirect-route path) (file-route path)))
      {:serve :nothing}))

(defn- shows-newest?
  "True when the blog with the settings `settings` shows its newest post on its home page."
  [settings]
  (= :newest-post (:home (templates/of-blog settings))))

;; The responses. Pure.

(def ^:private content-types
  {"html" "text/html; charset=utf-8"
   "css"  "text/css; charset=utf-8"
   "js"   "text/javascript; charset=utf-8"
   "map"  "application/json"
   "edn"  "application/edn; charset=utf-8"
   "svg"  "image/svg+xml"
   "png"  "image/png"
   "jpg"  "image/jpeg"
   "jpeg" "image/jpeg"
   "gif"  "image/gif"
   "webp" "image/webp"})

(def ^:private not-found
  {:status 404 :headers {"Content-Type" "text/plain; charset=utf-8"} :body "Not found."})

(defn- fresh
  "A response that sends `body` as the content of a file with the extension `extension`, and
   that no browser keeps."
  [extension body]
  {:status  200
   :headers {"Content-Type"  (get content-types extension "application/octet-stream")
             "Cache-Control" "no-store"}
   :body    body})

(defn- redirect [location]
  {:status 302 :headers {"Location" location}})

;; The edge. Each function below reads the disk.

(defn- serve-file
  "The response that sends the file at `path`, or a 404 when there is no such file."
  [path]
  (if (fs/regular-file? path)
    (fresh (fs/extension path) (fs/file path))
    not-found))

(defn- find-post-dir
  "The directory of the post `folder` in the blog at `root`, or nil when there is none."
  [root folder]
  (first (filter fs/directory? (map #(fs/path root % folder) layout/post-dirs))))

(defn- read-settings
  "The settings of the blog at `root`. Throws with the name of the file when it cannot be read."
  [root]
  (try
    (edn/read-string (slurp (fs/file root layout/settings-file)))
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
  "Every directory in the post directories of the blog at `root`, with its name, its place from
   the root, and whether it is a draft."
  [root]
  (for [dir   layout/post-dirs
        path  (when (fs/directory? (fs/path root dir)) (sort (fs/list-dir (fs/path root dir))))
        :when (fs/directory? path)]
    {:folder (fs/file-name path) :dir path :place (str dir "/" (fs/file-name path))
     :draft? (= dir layout/drafts-dir)}))

(defn- post-dir? [{:keys [folder]}]
  (post/folder? folder))

(defn- refused
  "What is wrong with a directory whose name is no name of a post folder."
  [{:keys [place]}]
  (str place " is left out. A post folder has small letters, digits and dashes in its name, "
       "and the blog keeps the names " (str/join ", " (sort address/reserved)) " for itself."))

(defn- read-entry
  "The entry of the post in a directory, and what is wrong with the post."
  [{:keys [dir draft? place] folder-name :folder}]
  (let [post (read-post folder-name dir)]
    {:entry    (site/entry folder-name post draft?)
     :problems (map #(str place ": " %) (:problems post))}))

(defn read-site
  "What every page of the blog at `root` loads."
  [root]
  (let [dirs (list-dirs root)
        read (map read-entry (filter post-dir? dirs))]
    (site/site (read-settings root)
               (map :entry read)
               (concat (map refused (remove post-dir? dirs)) (mapcat :problems read)))))

(defn- read-shown-post
  "The post in the folder `folder-name` as a page shows it: with the name of its folder, and
   with each file beside it under its full address, so that any page can show it. Nil when
   there is no such post."
  [root folder-name]
  (when-some [dir (some->> folder-name (find-post-dir root))]
    (-> (read-post folder-name dir)
        (post/placed folder-name)
        (assoc :folder folder-name))))

(defn- read-post-page [root {folder-name :folder :as page} _site]
  (when-some [post (read-shown-post root folder-name)]
    {:page page :post post}))

;; The settings say what the home page shows, and they are one small file. The list of posts
;; is read only when the home page shows a post, and nobody read it yet.
(defn- read-home-page
  "The data of a page of the home page. It holds the newest post when the blog template shows
   that on its home page."
  [root page site]
  (let [post (when (shows-newest? (or (:blog site) (read-settings root)))
               (read-shown-post root (site/newest-folder (or site (read-site root)))))]
    (cond-> {:page page} post (assoc :post post))))

(defn- read-plain-page [_root page _site]
  {:page page})

(def ^:private page-readers
  "What reads the data of each kind of page."
  {:post read-post-page
   :home read-home-page})

(defn read-page
  "The data of the page `page` of the blog at `root`: what the page is, and the post that it
   shows when it shows one. Nil when it is the page of a post that is not there. With `site`,
   the site as it was read, nothing of it is read again."
  ([root page]
   (read-page root page nil))
  ([root page site]
   ((get page-readers (:kind page) read-plain-page) root page site)))

;; The preview. A page of the preview is told one of two things: the site and its own data,
;; with the address of the socket that tells it again, or a problem to show in place of them.

(defn- problem
  "What a page is told when its data could not be read. The same line goes to the terminal."
  [^Throwable cause]
  (let [text (str "Could not read the blog. " (.getMessage cause))]
    (println text)
    {:problem text}))

(defn- without-throwing
  "What `read` gives, or a problem when it throws."
  [read]
  (try
    (read)
    (catch Exception cause
      (problem cause))))

(defn- read-preview
  "The data of the page `page`, with the address of its socket. It does not throw. Nil when it
   is the page of a post that is not there. `site` is the site as it was read, or nil."
  [root page site]
  (without-throwing #(some-> (read-page root page site) (assoc :changes wire/changes))))

(defn- gone [page]
  {:problem (str "There is no post " (:folder page) " any more.")})

(defn- read-told
  "What each of the open pages `pages` is told to show, by page. The site is read once. It does
   not throw."
  [root pages]
  (let [site (without-throwing #(read-site root))]
    (into {}
          (map (fn [page] [page (site/told site (or (read-preview root page site) (gone page)))]))
          pages)))

(defn- serve-site [root _found _request]
  (fresh "edn" (pr-str (without-throwing #(read-site root)))))

(defn- serve-data [root {:keys [page]} _request]
  (if-some [told (read-preview root page nil)]
    (fresh "edn" (pr-str told))
    not-found))

(defn- serve-changes [root {:keys [page]} request]
  (live/socket (partial read-told root) request page))

(defn- serve-media [root {:keys [folder file]} _request]
  (if-some [dir (find-post-dir root folder)]
    (serve-file (fs/path dir file))
    not-found))

(defn- serve-static [_root {:keys [file]} _request]
  (serve-file (fs/path public-dir file)))

(defn- serve-page [_root _found _request]
  (serve-file (fs/path public-dir "index.html")))

(defn- serve-redirect [_root {:keys [to]} _request]
  (redirect to))

(defn- serve-nothing
  "The page of the app, as a page that is not there. The app shows that there is nothing."
  [_root _found _request]
  (assoc (serve-file (fs/path public-dir "index.html")) :status 404))

(def ^:private servers
  "The function that serves each thing a route can ask for, from the blog root, what the
   route found and the request."
  {:site     serve-site
   :static   serve-static
   :page     serve-page
   :redirect serve-redirect
   :data     serve-data
   :changes  serve-changes
   :media    serve-media
   :nothing  serve-nothing})

(defn- handler [root request]
  (let [found (route (:uri request))]
    ((servers (:serve found)) root found request)))

(defn- announce!
  "Says where the blog at `root` is served, and what is wrong with it."
  [root port]
  (println (str "Serving the blog in " root " on http://localhost:" port "/"))
  (let [{:keys [problem problems]} (without-throwing #(read-site root))]
    (run! println (if problem [] problems))))

(defn- blog? [root]
  (and (some? root) (fs/regular-file? (fs/path root layout/settings-file))))

(defn- refuse! [root]
  (println (if root
             (str "No " layout/settings-file " in " root ". A blog has one at its top.")
             "Give the directory of a blog: bb serve example [port]"))
  (System/exit 1))

(defn -main
  "Serves the blog in the directory `root` until the process stops."
  [& [root port]]
  (when-not (blog? root)
    (refuse! root))
  (let [port (if port (parse-long port) default-port)]
    (http/run-server (partial handler root) {:port port})
    (live/watch! root (partial read-told root))
    (announce! root port)
    @(promise)))
