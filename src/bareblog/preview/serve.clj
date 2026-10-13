(ns bareblog.preview.serve
  "A local server for one blog: the app, and each post as data.

   What is served is a map: under `:blog` how the blog is read, as `bareblog.read.source` reads
   it, and under `:app` the directory of the files of the app. A blog that is read for its
   author has its pages told of every change, and of what is wrong with it."
  (:require [babashka.fs :as fs]
            [bareblog.blog.address :as address]
            [bareblog.blog.site :as site]
            [bareblog.blog.text :as text]
            [bareblog.blog.wire :as wire]
            [bareblog.preview.live :as live]
            [bareblog.read.source :as source]
            [bareblog.written.feed :as feed]
            [clojure.string :as str]
            [org.httpkit.server :as http])
  (:import [java.util.regex Matcher]))

(defn app-dir
  "The directory of the files of the app, for BareBlog in the directory `home`."
  [home]
  (fs/path home "resources" "public"))

;; What a path asks for. Pure.

(def ^:private folder
  (str "(" text/folder-pattern ")"))

(def ^:private file
  (str "(" text/file-pattern ")"))

(defn- escaped
  "The text `text` as the text of a pattern that matches it and nothing else."
  [text]
  (str/escape text {\. "\\."}))

(defn- home-page [[number]]
  {:kind :home :number (if number (parse-long number) 1)})

(defn- tag-page
  "The page of the tag that an address spells as `spelled`, or nil when its escapes are broken."
  [[spelled]]
  (when-some [tag-name (address/decoded spelled)]
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
    (when-some [file-name (text/file-beside spelled)]
      {:serve :media :folder folder-name :file file-name})))

(def ^:private top-files
  "What each file at the top of a blog that is no file of the app serves."
  {wire/site-data {:serve :site}
   wire/feed      {:serve :feed}
   wire/missing   {:serve :missing}})

(defn- file-route
  "What `path` serves when it is a file: of the site, of the app, or beside a post."
  [path]
  (or (top-files (subs path 1))
      (when-some [[static] (found-by static-pattern path)]
        {:serve :static :file static})
      (media-route path)))

(defn below
  "The path `path` of a request as a path from the top of a blog at `root`, or nil when it is
   not under it."
  [root path]
  (when (str/starts-with? path root)
    (str "/" (subs path (count root)))))

(defn above
  "The path `path` from the top of a blog as a path of a request, for a blog at `root`."
  [root path]
  (str root (subs path 1)))

;; The page of the app names its files from the top of a host. They are named from the top of
;; the blog here, and the document is told where that is.
(defn document
  "The page of the app, `html`, for a blog at `root`. With `missing?` it is the page for an
   address with no page, and says so."
  [html root missing?]
  (-> html
      (str/replace #"(href|src)=\"/" (str "$1=\"" (Matcher/quoteReplacement root)))
      (str/replace "<html " (str "<html " wire/root-attribute "=\"" root "\" "
                                 (when missing? (str wire/missing-attribute " "))))))

;; A path is matched as it is written, and what a pattern finds is read after that. A slash
;; that is spelled as an escape is then part of a name, and no slash of the path.
(defn route
  "What the server serves for the path `path` of a request, as a map with `:serve`. A path that
   names nothing, or that holds two dots in a row, serves `:nothing`."
  [path]
  (or (when-not (str/includes? path "..")
        (or (page-routes path) (redirect-route path) (file-route path)))
      {:serve :nothing}))

;; The responses. Pure.

(def ^:private content-types
  {"html" "text/html; charset=utf-8"
   "css"  "text/css; charset=utf-8"
   "js"   "text/javascript; charset=utf-8"
   "map"  "application/json"
   "edn"  "application/edn; charset=utf-8"
   "xml"  "application/atom+xml; charset=utf-8"
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

;; The edge. Each function below reads the disk, or answers a request.

(defn- serve-file
  "The response that sends the file at `path`, or a 404 when there is no such file."
  [path]
  (if (fs/regular-file? path)
    (fresh (fs/extension path) (fs/file path))
    not-found))

;; A page is told one of two things: the site and its own data, or a problem to show in place
;; of them. Where the pages of a blog are told of changes, the data of a page names the socket
;; that tells it.

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

(defn- with-socket
  "The data `data` of a page of `blog`, with the address of its socket where its pages are told
   of changes."
  [{:keys [preview?]} data]
  (cond-> data preview? (assoc :changes wire/changes)))

(defn- read-shown
  "The data of the page `page` as the page fetches it. It does not throw. Nil when it is the
   page of a post that is not there. `site` is the site as it was read, or nil."
  [blog page site]
  (without-throwing #(some->> (source/read-page blog page site) (with-socket blog))))

(defn- gone [page]
  {:problem (str "There is nothing named " (:folder page) " any more.")})

(defn- read-told
  "What each of the open pages `pages` is told to show, by page. The site is read once. It does
   not throw."
  [blog pages]
  (let [site (without-throwing #(source/read-site blog))]
    (into {}
          (map (fn [page] [page (site/told site (or (read-shown blog page site) (gone page)))]))
          pages)))

(defn- read-loaded
  "The site of `blog` as a page fetches it: for its author with what is wrong with it, and for
   its readers without. It does not throw."
  [{:keys [preview?] :as blog}]
  (cond-> (without-throwing #(source/read-site blog))
    (not preview?) site/published))

(defn- serve-site [{:keys [blog]} _found _request]
  (fresh "edn" (pr-str (read-loaded blog))))

(defn- serve-feed [{{:keys [root] :as blog} :blog} _found _request]
  (let [site (without-throwing #(source/read-site blog))]
    (if (:problem site)
      not-found
      (fresh "xml" (feed/atom-feed root site)))))

(defn- serve-data [{:keys [blog]} {:keys [page]} _request]
  (if-some [told (read-shown blog page nil)]
    (fresh "edn" (pr-str told))
    not-found))

(defn- serve-changes [{:keys [blog]} {:keys [page]} request]
  (if (:preview? blog)
    (live/socket (partial read-told blog) request page)
    not-found))

(defn- serve-media [{:keys [blog]} {:keys [folder file]} _request]
  (if-some [dir (source/find-dir blog folder)]
    (serve-file (fs/path dir file))
    not-found))

(defn- serve-static [{:keys [app]} {:keys [file]} _request]
  (serve-file (fs/path app file)))

(defn- read-document
  "The response that sends the page of the app, for what is served."
  [{:keys [app blog]} missing?]
  (fresh "html" (document (slurp (fs/file app wire/index)) (:root blog) missing?)))

(defn- serve-page [served _found _request]
  (read-document served false))

(defn- serve-missing [served _found _request]
  (read-document served true))

(defn- serve-redirect [{:keys [blog]} {:keys [to]} _request]
  (redirect (above (:root blog) to)))

(defn- serve-nothing
  "The page of the app, as the page for an address with no page."
  [served _found _request]
  (assoc (read-document served true) :status 404))

(def ^:private servers
  "The function that serves each thing a route can ask for, from what is served, what the
   route found and the request."
  {:site     serve-site
   :feed     serve-feed
   :static   serve-static
   :page     serve-page
   :missing  serve-missing
   :redirect serve-redirect
   :data     serve-data
   :changes  serve-changes
   :media    serve-media
   :nothing  serve-nothing})

(defn handler
  "The response to `request` of what `served` says is served. A path that is not under the top
   of the blog serves nothing."
  [{:keys [blog] :as served} request]
  (let [found (or (some-> (below (:root blog) (:uri request)) route) {:serve :nothing})]
    ((servers (:serve found)) served found request)))

(defn- announce!
  "Says where `blog` is served, and what is wrong with it."
  [{:keys [dir] :as blog} port]
  (println (str "Serving the blog in " (fs/canonicalize dir) " on http://localhost:" port "/"))
  (let [{:keys [problem problems]} (without-throwing #(source/read-site blog))]
    (run! println (if problem [] problems))))

(defn start!
  "Serves the blog in the directory `dir` on the port `port`, as a preview, with the files of
   the app in the directory `app`, and says where."
  [app dir port]
  (let [blog (source/preview dir)]
    (http/run-server (partial handler {:blog blog :app app}) {:port port})
    (live/watch! dir (partial read-told blog))
    (announce! blog port)))
