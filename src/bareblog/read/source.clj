(ns bareblog.read.source
  "Reads a blog from its directory: its settings, its posts, and the data of each page.

   The edge of the command, for both the preview and the generator. A blog is read as a map
   says: its directory under `:dir`, the top of its addresses under `:root`, and under
   `:preview?` whether it is read for its author, with its drafts, or for its readers.
   `preview` and `published` give the two maps there are.

   A blog that is read for its readers may say more. `:settings` are its settings, in place of
   the file. `:read` is the set of folders that are read, of posts and of pages, where it is
   not all of them. `:as-published` is the set of those that are taken as they were published
   last, from the output directory, and not read. `:comments` gives the comments of a post among
   those, by its folder, that it shows in place of those it was published with. `:pictures-in`
   is the directory, from its top, that holds its pictures, where that is not the top. `:dates`
   gives the date of each post and page, which is the moment it was first published."
  (:require [babashka.fs :as fs]
            [bareblog.blog.address :as address]
            [bareblog.blog.comments :as comments]
            [bareblog.blog.font :as font]
            [bareblog.blog.series :as series]
            [bareblog.blog.shape :as shape]
            [bareblog.blog.site :as site]
            [bareblog.blog.text :as text]
            [bareblog.blog.wire :as wire]
            [bareblog.blog.layout :as layout]
            [bareblog.blog.pictures :as pictures]
            [bareblog.read.image :as image]
            [bareblog.read.reader :as reader]
            [bareblog.written.output :as output]
            [clojure.edn :as edn]
            [clojure.java.io :as io])
  (:import [java.security MessageDigest]))

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
   it is in under `:in`. Nil when there is none, or when it holds no text."
  [{:keys [dir] :as blog} folder]
  (first (for [in    (read-dirs blog)
               :let  [path (fs/path dir in folder)]
               :when (fs/regular-file? (fs/path path (layout/text-file in)))]
           {:folder folder :dir path :in in})))

(defn find-dir
  "The directory of the post or the page `folder` in `blog`, or nil when there is none."
  [blog folder]
  (:dir (find-source blog folder)))

(defn- standalone?
  "True when what is in the directory `in` is a page that is no post."
  [{:keys [in]}]
  (= layout/pages-dir in))

(defn- read-edn-file
  "The value in the file `file-name` of the blog in the directory `dir`. Throws with the name of
   the file when it cannot be read."
  [dir file-name]
  (try
    (edn/read-string (slurp (fs/file dir file-name)))
    (catch Exception problem
      (throw (ex-info (str file-name ": " (.getMessage problem)) {} problem)))))

(defn read-settings
  "The settings of the blog in the directory `dir`. Throws with the name of the file when it
   cannot be read."
  [dir]
  (read-edn-file dir layout/settings-file))

(defn read-series
  "The series of the blog in the directory `dir`, which are none when it has no file of them.
   Throws with the name of the file when it cannot be read."
  [dir]
  (if-not (fs/regular-file? (fs/file dir layout/series-file))
    {}
    (let [read (read-edn-file dir layout/series-file)]
      (when-not (series/well-formed? read)
        (throw (ex-info (str layout/series-file " is not as bareblog series writes it. Make the "
                             "series anew with bareblog series.")
                        {})))
      read)))

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

;; The comments of a post are beside its text. What cannot be read of them is a problem of the
;; blog, and the post then shows none, so that a page never shows half a thread. Only a post
;; has comments.
(defn- read-comments
  "The comments beside the post in `source`, and what is wrong with them, as texts. No file,
   and a page that is no post, is no comments."
  [{:keys [dir place] :as source}]
  (let [file (fs/file dir layout/comments-file)]
    (if (or (standalone? source) (not (fs/regular-file? file)))
      {:comments []}
      (let [read  (try (edn/read-string (slurp file)) (catch Exception _ ::unreadable))
            found (if (= ::unreadable read)
                    [(str "The comments of " place " cannot be read.")]
                    (comments/problems place read))]
        (if (seq found) {:comments [] :problems found} {:comments read})))))

(defn- shown-comment
  "The `comment` as a page shows it: its text as blocks, and its short id where `blog` is read
   for its author."
  [{:keys [preview?]} {:keys [id text] :as comment}]
  (cond-> (-> comment (dissoc :text) (assoc :blocks (reader/read-comment text)))
    preview? (assoc :short-id (comments/short-id id))))

(defn read-post-comments
  "The comments of the post in `folder` of the blog in the directory `dir`, and what is wrong
   with them, as texts."
  [dir folder]
  (read-comments {:dir   (fs/path dir layout/posts-dir folder)
                  :place (str layout/posts-dir "/" folder)
                  :in    layout/posts-dir}))

(defn- with-comments
  "The text `text` of `blog` with `comments` as a page shows them, and none when there are none."
  [blog text comments]
  (let [shown-comments (mapv (partial shown-comment blog) comments)]
    (if (seq shown-comments)
      (assoc text :comments shown-comments)
      (dissoc text :comments))))

(defn- read-kept-page
  "The data of the page of `folder` of `blog` as it was published last, with the comments that
   `blog` gives it in place of those it had. Nil when it was not published."
  [{:keys [dir comments] :as blog} folder]
  (some-> (read-published-page dir folder)
          (cond-> (contains? comments folder)
            (update :text (partial with-comments blog) (get comments folder)))))

(defn- named-well? [{:keys [folder]}]
  (text/folder? folder))

;; A post and a page of the same name would have the same address. The post is the one that
;; is shown, and the author is told. A folder of the posts with no text is no post.
(defn- with-shadows
  "The directories `dirs`, each page with the name of a post marked as shadowed."
  [dirs]
  (let [posts (into #{} (comp (remove standalone?) (filter :text?) (map :folder)) dirs)]
    (map (fn [source]
           (cond-> source
             (and (standalone? source) (contains? posts (:folder source))) (assoc :shadowed? true)))
         dirs)))

(defn- list-dirs
  "Every directory in the directories that `blog` reads, with its name, its place from the top
   of the blog, the directory that it is in, whether it is a draft, whether its text is there,
   and whether it is a page with the name of a post."
  [{:keys [dir] :as blog}]
  (with-shadows
    (for [in    (read-dirs blog)
          path  (when (fs/directory? (fs/path dir in)) (sort (fs/list-dir (fs/path dir in))))
          :when (fs/directory? path)]
      {:folder (fs/file-name path) :dir path :in in :place (str in "/" (fs/file-name path))
       :draft? (= in layout/drafts-dir)
       :text?  (fs/regular-file? (fs/path path (layout/text-file in)))})))

(def ^:private usable?
  "True when a directory can be read as a post or a page."
  (every-pred named-well? :text? (complement :shadowed?)))

(defn list-folders
  "The folders of `blog` that can be read, as a set under `:posts` and one under `:pages`."
  [blog]
  (let [usable (filter usable? (list-dirs blog))]
    {:posts (into #{} (comp (remove standalone?) (map :folder)) usable)
     :pages (into #{} (comp (filter standalone?) (map :folder)) usable)}))

(defn list-texts
  "Each post, draft and page of the blog in the directory `dir` that can be read: its folder,
   its place from the top of the blog, its file, and whether it is a draft or a page."
  [dir]
  (for [{:keys [in] folder-dir :dir :as source} (filter usable? (list-dirs (preview dir)))]
    (assoc (select-keys source [:folder :place :draft?])
           :page? (standalone? source)
           :file  (str (fs/path folder-dir (layout/text-file in))))))

(defn- refused
  "What is wrong with a directory whose name is no name of the folder of a post or a page."
  [{:keys [place]}]
  (str place " is left out. " text/folder-rule))

(defn- shadowed-problem [{:keys [place folder]}]
  (str place " is left out. There is a post " folder " too, and both would have the same address."))

(defn- textless-problem [{:keys [place in]}]
  (str place " is left out. It has no " (layout/text-file in) "."))

(defn- folder-problems
  "What is wrong with the directories `dirs` themselves, as texts."
  [dirs]
  (concat (map refused (remove named-well? dirs))
          (map textless-problem (filter (every-pred named-well? (complement :text?)) dirs))
          (map shadowed-problem (filter :shadowed? dirs))))

(defn- unpublished
  "The entry `entry` of a page, marked as not published when `blog` is read for its author and
   the page was not published."
  [{:keys [preview? published-pages]} entry]
  (cond-> entry
    (and preview? (not (contains? published-pages (:folder entry)))) (assoc :unpublished? true)))

;; A text that shows or links to a file beside it that is not there has a page with a hole
;; in it. The author is told, as of the other things that are wrong with a blog.
(defn- list-missing
  "What is wrong with the files of the text `read` in `source`, as texts: each file that it
   shows or links to and that is not beside it."
  [read {:keys [dir place]}]
  (for [file-name (sort (text/files read))
        :when     (not (fs/regular-file? (fs/path dir file-name)))]
    (str place " names " file-name ", and there is no such file beside it.")))

;; The pictures of a blog are beside its settings, which name them. What is read of each is
;; its name, its size where it has one, and a version made from what it holds.
(defn pictures-dir
  "The directory that holds the pictures of `blog`."
  [{:keys [dir pictures-in]}]
  (cond-> (fs/path dir)
    pictures-in (fs/path pictures-in)))

(defn version
  "The version of the bytes `bytes`: the start of a digest of them."
  [^bytes bytes]
  (let [digest (.digest (MessageDigest/getInstance "SHA-256") bytes)]
    (subs (format "%064x" (BigInteger. 1 digest)) 0 12)))

(defn read-version
  "The version of the file at `path`: the start of a digest of what it holds."
  [path]
  (version (fs/read-all-bytes path)))

(defn- find-picture-file
  "The file of `picture` in the directory `in`, or nil."
  [in {:keys [src] :as picture}]
  (when (pictures/fits? picture)
    (let [path (fs/path in src)]
      (when (fs/regular-file? path)
        path))))

(defn- read-picture
  "What `blog` holds of `picture`: its name under `:src`, its version, and its size where it
   has one. Nil when the file is not there."
  [blog {:keys [src] :as picture}]
  (when-some [path (find-picture-file (pictures-dir blog) picture)]
    (merge {:src src :version (read-version path)} (read-size path))))

(defn- with-picture
  "`found` with what `blog` holds of `picture`, where it holds it."
  [blog found {:keys [at] :as picture}]
  (if-some [read (read-picture blog picture)]
    (assoc-in found at read)
    found))

(defn- read-pictures
  "The pictures of `blog` with the settings `settings`, by kind and by mode."
  [blog settings]
  (reduce (partial with-picture blog) {} (pictures/named settings)))

(defn find-served-picture
  "The file of the picture that the settings of `blog` name `file-name`, or nil."
  [blog file-name]
  (some (partial find-picture-file (pictures-dir blog))
        (filter (comp #{file-name} :src) (pictures/named (settings-of blog)))))

(defn- same-file?
  "True when the files at `a` and `b` hold the same bytes."
  [a b]
  (java.util.Arrays/equals (fs/read-all-bytes a) (fs/read-all-bytes b)))

(defn find-changed-pictures
  "The name of each picture that the settings `settings` of the blog in `dir` name, where the
   file beside them is not the one that was published."
  [dir settings]
  (distinct
   (for [picture (pictures/named settings)
         :let    [written   (find-picture-file dir picture)
                  published (find-picture-file (fs/path dir layout/output-dir) picture)]
         :when   (and written (not (and published (same-file? written published))))]
     (:src picture))))

(defn- picture-problem
  "What is wrong with `picture`, a picture that `blog` does not hold, as a text."
  [{:keys [pictures-in]} {:keys [src] :as picture}]
  (str layout/settings-file " names " (pr-str src) " as its " (pictures/kind-of picture) ", "
       (cond
         (not (pictures/fits? picture))
         (str "which is no name of " (pictures/said picture) " beside " layout/settings-file)

         pictures-in
         (str "and there is no such file in " pictures-in ", where it was published")

         :else
         (str "and there is no such file beside " layout/settings-file))
       "."))

(defn- picture-problems
  "What is wrong with the pictures that `settings` name, as texts, when `found` is what was
   read of them in `blog`."
  [blog settings found]
  (for [{:keys [at] :as picture} (pictures/named settings)
        :when                    (nil? (get-in found at))]
    (picture-problem blog picture)))

(defn find-font-dir
  "The folder that holds the font of `blog`, as its settings or `settings` name it among its
   fonts, or nil when they name none or it has no folder."
  ([blog]
   (find-font-dir blog (settings-of blog)))
  ([blog settings]
   (when-some [family (font/family settings)]
     (let [dir (fs/path (pictures-dir blog) (font/folder family))]
       (when (fs/directory? dir)
         dir)))))

(defn- find-faces-file
  "The stylesheet of the faces of the font that `settings` name for `blog`, or nil."
  [blog settings]
  (when-some [dir (find-font-dir blog settings)]
    (let [path (fs/path dir font/faces-file)]
      (when (fs/regular-file? path)
        path))))

(defn read-font-css
  "The stylesheet of the font of `blog`."
  [blog]
  (let [settings (settings-of blog)]
    (font/stylesheet (font/family settings) (some-> (find-faces-file blog settings) fs/file slurp))))

(defn find-served-font-file
  "The file `file-name` in the folder `folder-name` among the fonts of `blog`, when that is
   the folder of its font, or nil."
  [blog folder-name file-name]
  (when-some [dir (find-font-dir blog)]
    (let [path (fs/path dir file-name)]
      (when (and (= folder-name (str (fs/file-name dir))) (fs/regular-file? path))
        path))))

(defn- font-problems
  "What is wrong with the font that `settings` name for `blog`, as texts: a folder of it that
   holds no faces."
  [blog settings]
  (when (and (find-font-dir blog settings) (nil? (find-faces-file blog settings)))
    (let [family (font/family settings)]
      [(str (font/folder family) " holds no " font/faces-file ", so the font is not served. "
            "Fetch it again with: bareblog font " (pr-str family))])))

(defn- read-entry
  "The entry of the post or the page of `blog` in `source`, with whether it is a page, and
   what is wrong with it."
  [blog {:keys [folder draft?] :as source}]
  (let [read                        (read-text blog source)
        {:keys [comments problems]} (read-comments source)]
    {:page?    (standalone? source)
     :entry    (if (standalone? source)
                 (unpublished blog (site/page-entry folder read))
                 (site/with-comment-count (site/entry folder read draft?) (count comments)))
     :problems (concat (list-missing read source) problems)}))

(defn- read?
  "True when `blog` reads `source`: it can be read, and is among the folders that the blog
   reads, where it does not read them all."
  [{:keys [read]} {:keys [folder] :as source}]
  (and (usable? source) (or (nil? read) (contains? read folder))))

(defn- read-entries
  "The entry of each post and each page that `blog` reads among the directories `dirs`, with
   what is wrong with it."
  [blog dirs]
  (map (partial read-entry blog) (filter (partial read? blog) dirs)))

(defn- with-kept-count
  "The entry `entry`, taken as it was published, with the number of the comments that `comments`
   gives its post in place of those it had."
  [comments {:keys [folder] :as entry}]
  (cond-> entry
    (contains? comments folder) (site/with-comment-count (count (get comments folder)))))

(defn- read-kept
  "The entries under `k`, `:posts` or `:pages`, that `blog` takes as they were published last,
   with the number of the comments that `blog` gives a post."
  [{:keys [dir as-published comments]} k]
  (when (seq as-published)
    (->> (get (read-published dir) k)
         (filter (comp as-published :folder))
         (map (partial with-kept-count comments)))))

;; A series is checked against the posts and the drafts on disk, also when the blog is read for
;; its readers, so that a part that is still a draft is no problem.
(defn- series-problems
  "What is wrong with `series`, the series of the blog in the directory `dir`, as texts."
  [dir series]
  (series/problems series (:posts (list-folders (preview dir)))))

(defn read-problems
  "What is wrong with the posts and the pages that `blog` reads, as texts: with the files
   they name, with the tags of the posts, also beside those taken as they were published, and
   with the series of the blog."
  [blog]
  (let [read (read-entries (with-published blog) (list-dirs blog))
        kept (read-kept blog :posts)]
    (concat (mapcat :problems read)
            (output/problems {:posts (concat (map :entry (remove :page? read)) kept)}
                             (into #{} (map :folder) kept))
            (series-problems (:dir blog) (read-series (:dir blog))))))

(defn read-site
  "What every page of `blog` loads, with what is wrong with the blog. Its `:series` hold the
   parts that are among its posts."
  [blog]
  (let [blog     (with-published blog)
        settings (settings-of blog)
        series   (read-series (:dir blog))
        dirs     (list-dirs blog)
        read     (read-entries blog dirs)
        found    (read-pictures blog settings)
        site     (-> (site/site settings
                                (concat (map :entry (remove :page? read)) (read-kept blog :posts))
                                (concat (folder-problems dirs) (mapcat :problems read)
                                        (site/settings-problems settings)
                                        (picture-problems blog settings found)
                                        (font-problems blog settings)
                                        (series-problems (:dir blog) series)))
                     (site/with-pages (concat (map :entry (filter :page? read)) (read-kept blog :pages))))
        shown    (series/shown series (:posts site))]
    (cond-> (site/with-problems site (output/problems site))
      (seq found) (assoc :pictures found)
      (seq shown) (assoc :series shown))))

;; What is taken as it was published has its files from where they were published. The
;; output directory is still there while new pages are written beside it.
(defn find-media-dir
  "The directory that holds the files beside the post or the page in `folder` of `blog`."
  [{:keys [dir as-published] :as blog} folder]
  (if (contains? as-published folder)
    (fs/path dir layout/output-dir folder)
    (find-dir blog folder)))

;; The preview sends a file and the generator copies it by this one answer, so that what an
;; author sees of a file in the preview is what is published of it.
(defn files-of
  "The names of the files beside `shown`, a text of `blog` as a page shows it, that it shows
   or links to, as a set. These are published with it, and no other."
  [{:keys [root]} shown]
  (text/files shown (address/post root (:folder shown))))

(defn- shown
  "The text of `blog` in `source` as a page shows it: with the name of its folder, with each
   file beside it under its full address, so that any page can show it, and with its
   comments."
  [{:keys [root] :as blog} {:keys [folder] :as source}]
  (with-comments blog
                 (-> (read-text (with-published blog) source)
                     (text/placed (address/post root folder))
                     (assoc :folder folder))
                 (:comments (read-comments source))))

(defn read-shown-text
  "The post or the page in the folder `folder` as a page shows it. One that `blog` takes as it
   was published is the one that was published. Nil when there is none."
  [{:keys [as-published] :as blog} folder]
  (if (contains? as-published folder)
    (:text (read-kept-page blog folder))
    (some->> folder (find-source blog) (shown blog))))

;; The address of a post and of a page that is no post look the same, so a page is asked for
;; as a post. What is in the folder says which of the two it is.
(defn- read-text-page
  "The data of the page of the post or the page in the folder of `page`: what the page is,
   with its kind, and its text."
  [{:keys [as-published] :as blog} {folder :folder :as page} _site]
  (if (contains? as-published folder)
    (read-kept-page blog folder)
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
