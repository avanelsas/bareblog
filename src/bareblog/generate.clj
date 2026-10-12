(ns bareblog.generate
  "Writes a whole blog out as static pages, in its `docs` directory.

   The edge of the command. What the blog holds is `bareblog.output`, which is pure. The blog is
   served here without its drafts, at the address in its settings, and a saver opens each page
   in a browser and writes it out with its components drawn.

   `generate!` is given how: `:save`, which saves the page at an address into a file, `:say`,
   which is told each thing that happens as a text, and `:app`, the directory of the files of
   the app."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [bareblog.address :as address]
            [bareblog.home :as home]
            [bareblog.layout :as layout]
            [bareblog.output :as output]
            [bareblog.post :as post]
            [bareblog.serve :as serve]
            [bareblog.source :as source]
            [bareblog.wire :as wire]
            [clojure.string :as str]
            [org.httpkit.server :as http]))

(def ^:private saver-setting
  "The name of the setting of the environment that says where the saver is."
  "BAREDOM_PRERENDER")

(defn- saver-beside
  "Where the saver is when BareDOM is checked out beside BareBlog, which is in `home`."
  [home]
  (fs/path home ".." "BareDom" "scripts" "prerender.mjs"))

(def ^:private window
  "The window that a page is saved in, as a flag for the browser. It is a wide one, so that a
   saved page is the page of a wide window whatever the browser would open by itself. A flag
   of the author comes after it, and wins."
  "--window-size=1280,900")

(defn- find-saver
  "The script that saves a page, for BareBlog in `home`, or nil when there is none."
  [home]
  (first (filter fs/regular-file? (remove nil? [(System/getenv saver-setting) (saver-beside home)]))))

(defn- find-refusal
  "Why nothing can be generated with the script `saver` and the app in the directory `app`, as
   a text, or nil when it can."
  [saver app]
  (cond
    (nil? saver)
    (str "The saver was not found. Check BareDOM out beside BareBlog, or set " saver-setting
         " to its scripts/prerender.mjs.")

    (not (fs/regular-file? (fs/path app "js" "app.js")))
    "The app is not built. Where BareBlog is, run: npx shadow-cljs release app"

    (fs/directory? (fs/path app "js" "cljs-runtime"))
    (str "The app is a development build. Stop shadow-cljs watch, remove " (fs/path app "js")
         ", and run: npx shadow-cljs release app")))

(defn- last-line [text]
  (last (remove str/blank? (str/split-lines (str text)))))

(defn- save-with
  "A function that saves the page at an address into a file, with the script `saver`. It
   throws with what the script said when the page could not be saved."
  [saver]
  (fn [address file]
    (let [flags              (str/trim (str window " " (System/getenv "CHROME_FLAGS")))
          {:keys [exit err]} (process/shell {:out :string :err :string :continue true
                                             :extra-env {"CHROME_FLAGS" flags}}
                                            "node" (str saver) address (str file))]
      (when-not (zero? exit)
        (throw (ex-info (or (last-line err) "The saver stopped.") {}))))))

(defn- write-text! [out file text]
  (let [path (fs/path out file)]
    (fs/create-dirs (fs/parent path))
    (spit (fs/file path) text)))

(defn- save-page!
  "Saves the page `page` of `blog`, which is served at `origin`, into `out`, and writes its
   data beside it. Gives the file of the page."
  [{:keys [save say]} {:keys [root] :as blog} site origin out page]
  (let [file   (output/file-of root page)
        target (fs/path out file)]
    (fs/create-dirs (fs/parent target))
    (try
      (save (str origin (address/of-page root page)) target)
      (catch Exception problem
        (throw (ex-info (str "Could not save " file ". " (ex-message problem)) {} problem))))
    (when-some [data-file (output/data-of root page)]
      (write-text! out data-file (pr-str (source/read-page blog page site))))
    (say (str "  " file))
    file))

(defn- copy-app!
  "Copies the files of the app in the directory `app` into `out`: every file but its page,
   which each page is saved from."
  [app out]
  (doseq [path  (fs/glob app "**")
          :let  [file (fs/relativize app path)]
          :when (and (fs/regular-file? path) (not= wire/index (str file)))]
    (fs/create-dirs (fs/parent (fs/path out file)))
    (fs/copy path (fs/path out file) {:replace-existing true})))

;; What a file beside a post is, is one rule, and the server has the same. A file that a page
;; could not name is not copied, so that none takes the place of a page or of its data.
(defn- copy-media!
  "Copies the files beside each post in `site`, of `blog`, into `out`."
  [blog out {:keys [posts]}]
  (doseq [{:keys [folder]} posts
          path             (fs/list-dir (source/find-post-dir blog folder))
          :when            (and (fs/regular-file? path)
                                (not= layout/post-file (fs/file-name path))
                                (post/file-name? (fs/file-name path)))]
    (fs/create-dirs (fs/path out folder))
    (fs/copy path (fs/path out folder (fs/file-name path)) {:replace-existing true})))

(defn- write-blog!
  "Writes the whole of `blog` into the directory `out`, and gives the file of each page. Its
   pages are saved from a server that runs for as long as this takes."
  [{:keys [say app] :as how} {:keys [root] :as blog} out]
  (let [site   (source/read-site blog)
        server (http/run-server (partial serve/handler {:blog blog :app app}) {:port 0 :legacy-return-value? false})
        origin (str "http://localhost:" (http/server-port server))]
    (try
      (run! say (:problems site))
      (let [files (mapv (partial save-page! how blog site origin out) (output/pages site))]
        (doseq [[file text] (output/texts root site)]
          (write-text! out file text))
        (copy-app! app out)
        (copy-media! blog out site)
        files)
      (finally
        @(http/server-stop! server)))))

(def ^:private docs "docs")

;; The old pages are moved aside before the new ones take their place, and are put back when
;; that fails, so that there is never a moment with no pages.
(defn- replace!
  "Puts the directory `new` in the place of the directory `old`."
  [new old]
  (let [aside (fs/path (str old ".old"))]
    (fs/delete-tree aside)
    (when (fs/exists? old)
      (fs/move old aside))
    (try
      (fs/move new old)
      (catch Exception problem
        (when (fs/exists? aside)
          (fs/move aside old))
        (throw problem)))
    (fs/delete-tree aside)))

;; The pages are written beside `docs`, and take its place when every one of them is there. A
;; run that fails leaves the pages that were there as they were.
(defn generate!
  "Writes the blog in the directory `dir` into its `docs` directory, in place of what was
   there, as `how` says, and gives the file of each page."
  [{:keys [say] :as how} dir]
  (let [settings (source/read-settings dir)
        blog     (source/published dir settings)
        out      (fs/path dir (str docs ".new"))]
    (say (str "Generating the blog in " dir " for " (:root blog)))
    (when-not (:address settings)
      (say "The settings name no :address, so the blog is written for the top of its host."))
    (when (fs/exists? out)
      (say (str "Removing " out ", which an earlier run left.")))
    (fs/delete-tree out)
    (try
      (let [files (write-blog! how blog out)]
        (replace! out (fs/path dir docs))
        files)
      (finally
        (fs/delete-tree out)))))

(defn- generate-or-say!
  "Generates the blog in `dir`, and says how it went. True when it went well."
  [how dir]
  (let [started (System/currentTimeMillis)]
    (try
      (println (str (count (generate! how dir)) " pages in " (fs/path dir docs) ", in "
                    (quot (- (System/currentTimeMillis) started) 1000) " seconds."))
      true
      (catch Exception problem
        (println (ex-message problem))
        (println (str "Nothing was changed in " (fs/path dir docs) "."))
        false))))

(defn -main
  "Generates the blog in the directory `dir`, for BareBlog in the directory `home`."
  [home & [dir]]
  (let [saver   (find-saver home)
        app     (home/app-dir home)
        refusal (cond
                  (nil? dir)               "Give the directory of a blog: bb generate example"
                  (not (source/blog? dir)) (source/refusal dir)
                  :else                    (find-refusal saver app))]
    (when refusal
      (println refusal)
      (System/exit 1))
    (System/exit (if (generate-or-say! {:save (save-with saver) :say println :app app} dir) 0 1))))
