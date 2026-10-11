(ns bareblog.serve
  "A local server for one blog: the app, and each post as data."
  (:require [babashka.fs :as fs]
            [bareblog.image :as image]
            [bareblog.layout :as layout]
            [bareblog.live :as live]
            [bareblog.post :as post]
            [bareblog.reader :as reader]
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

(def ^:private data
  (str/escape wire/page-data {\. "\\."}))

(defn- path-pattern [& pieces]
  (re-pattern (apply str pieces)))

(def ^:private routes
  "The pattern of each path, what it serves, and the names of what the pattern finds. They are
   tried in order."
  [[#"/(js/[\w./-]+|[\w-]+\.\w+)"     :static   [:file]]
   [(path-pattern "/" folder)                  :redirect [:folder]]
   [(path-pattern "/" folder "/")              :page     [:folder]]
   [(path-pattern "/" folder "/" data)         :data     [:folder]]
   [(path-pattern "/" folder "/" wire/changes) :changes  [:folder]]
   [(path-pattern "/" folder "/" file)         :media    [:folder :file]]])

(defn- matched
  "What a route serves for `path`, or nil when its pattern does not match."
  [path [pattern serve names]]
  (when-some [[_ & found] (re-matches pattern path)]
    (assoc (zipmap names found) :serve serve)))

(defn route
  "What the server serves for the path `path` of a request, as a map with `:serve`. A percent
   escape in the path is read as its character. A path that names nothing, or that holds two
   dots in a row, serves `:nothing`."
  [path]
  (let [plain (post/decoded path)]
    (or (when (and plain (not (str/includes? plain "..")))
          (some (partial matched plain) routes))
        {:serve :nothing})))

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

(defn- read-page
  "The data of the page of the post `folder` in the blog at `root`: the settings of the blog
   and the post. Nil when there is no such post."
  [root folder]
  (when-some [dir (find-post-dir root folder)]
    {:blog (read-settings root)
     :post (read-post folder dir)}))

;; The preview. A page of the preview is told one of two things: its data, with the address
;; of the socket that tells it again, or a problem to show in place of them.

(defn- problem
  "What a page is told when its data could not be read. The same line goes to the terminal."
  [folder ^Throwable cause]
  (let [text (str "Could not read the post " folder ". " (.getMessage cause))]
    (println text)
    {:problem text}))

(defn- read-preview
  "What the page of the post `folder` is told to show. It does not throw. Nil when there is no
   such post."
  [root folder]
  (try
    (some-> (read-page root folder) (assoc :changes wire/changes))
    (catch Exception cause
      (problem folder cause))))

(defn- gone [folder]
  {:problem (str "There is no post " folder " any more.")})

(defn- read-told
  "What the open page of the post `folder` is told to show, also when its folder is gone."
  [root folder]
  (or (read-preview root folder) (gone folder)))

(defn- serve-data [root {:keys [folder]} _request]
  (if-some [told (read-preview root folder)]
    (fresh "edn" (pr-str told))
    not-found))

(defn- serve-changes [root {:keys [folder]} request]
  (live/socket (partial read-told root) request folder))

(defn- serve-media [root {:keys [folder file]} _request]
  (if-some [dir (find-post-dir root folder)]
    (serve-file (fs/path dir file))
    not-found))

(defn- serve-static [_root {:keys [file]} _request]
  (serve-file (fs/path public-dir file)))

(defn- serve-page [_root _found _request]
  (serve-file (fs/path public-dir "index.html")))

(defn- serve-redirect [_root {:keys [folder]} _request]
  (redirect (str "/" folder "/")))

(defn- serve-nothing [_root _found _request]
  not-found)

(def ^:private servers
  "The function that serves each thing a route can ask for, from the blog root, what the
   route found and the request."
  {:static   serve-static
   :page     serve-page
   :redirect serve-redirect
   :data     serve-data
   :changes  serve-changes
   :media    serve-media
   :nothing  serve-nothing})

(defn- handler [root request]
  (let [found (route (:uri request))]
    ((servers (:serve found)) root found request)))

(defn- list-folders
  "The names of the post folders of the blog at `root`, in order."
  [root]
  (sort (for [dir    layout/post-dirs
              folder (when (fs/directory? (fs/path root dir)) (fs/list-dir (fs/path root dir)))
              :when  (fs/directory? folder)]
          (fs/file-name folder))))

(defn- announce! [root port]
  (println (str "Serving the blog in " root " on http://localhost:" port))
  (run! #(println (str "  http://localhost:" port "/" % "/")) (list-folders root)))

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
