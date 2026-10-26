(ns bareblog.publish.generate
  "Writes a whole blog out as static pages, in its `docs` directory.

   The edge of the command. What the blog holds is `bareblog.written.output`, which is pure.
   The blog is served here without its drafts, at the address in its settings, and a saver
   opens each page in a browser and writes it out with its components drawn.

   `generate!` is given how: `:save`, which saves pages, each the `:address` it is at and the
   `:file` it goes into, and calls a function with each file as it is saved, `:say`,
   which is told each thing that happens as a text, and `:app`, the directory of the files of
   the app. `ready` gives how for BareBlog in a directory, or why nothing can be generated."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [bareblog.blog.address :as address]
            [bareblog.blog.wire :as wire]
            [bareblog.blog.layout :as layout]
            [bareblog.blog.pictures :as pictures]
            [bareblog.preview.serve :as serve]
            [bareblog.read.source :as source]
            [bareblog.written.output :as output]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [org.httpkit.server :as http]))

(def ^:private saver-setting
  "The name of the setting of the environment that says where the saver is."
  "BAREDOM_PRERENDER")

(def ^:private saver-script (fs/path "scripts" "prerender.mjs"))

(defn- saver-installed
  "Where the saver is when BareBlog, which is in `home`, has the package of BareDOM installed."
  [home]
  (fs/path home "node_modules" "@vanelsas" "baredom" saver-script))

(defn- saver-beside
  "Where the saver is when BareDOM is checked out beside BareBlog, which is in `home`."
  [home]
  (fs/path home ".." "BareDom" saver-script))

(def ^:private window
  "The window that a page is saved in, as a flag for the browser. It is a wide one, so that a
   saved page is the page of a wide window whatever the browser would open by itself. A flag
   of the author comes after it, and wins."
  "--window-size=1280,900")

(defn saver-places
  "Where the saver may be, for BareBlog in `home`, in the order they are looked in: where the
   environment says with `setting`, in the package of BareDOM that BareBlog has installed, and
   in a BareDOM beside BareBlog."
  [home setting]
  (remove nil? [setting (saver-installed home) (saver-beside home)]))

(defn- find-saver
  "The script that saves a page, for BareBlog in `home`, or nil when there is none."
  [home]
  (first (filter fs/regular-file? (saver-places home (System/getenv saver-setting)))))

(defn- find-refusal
  "Why nothing can be generated with the script `saver` and the app in the directory `app`, as
   a text, or nil when it can."
  [saver app]
  (cond
    (nil? saver)
    (str "The saver was not found. Where BareBlog is, run npm install, or check BareDOM out "
         "beside BareBlog, or set " saver-setting " to its " saver-script ".")

    (not (fs/regular-file? (fs/path app "js" "app.js")))
    "The app is not built. Where BareBlog is, run: npx shadow-cljs release app"

    (fs/directory? (fs/path app "js" "cljs-runtime"))
    (str "The app is a development build. Stop shadow-cljs watch, remove " (fs/path app "js")
         ", and run: npx shadow-cljs release app")))

(defn- last-line [text]
  (last (remove str/blank? (str/split-lines (str text)))))

(def ^:private pages-a-run
  "How many pages the saver is given in one run, so that its arguments stay within what a
   command may be given."
  200)

(defn- saved-file
  "The file that a line of what the saver says names, or nil when the line names none."
  [line]
  (second (re-matches #"(.+): \d+ characters, \d+ shadow roots" line)))

(defn- run-saver!
  "Runs the script `saver` for `pages`, and calls `saved!` with each file as the script says
   that it is written. Throws with what the script said when a page could not be saved."
  [saver pages saved!]
  (let [flags (str/trim (str window " " (System/getenv "CHROME_FLAGS")))
        run   (apply process/process
                     {:err :string :extra-env {"CHROME_FLAGS" flags}}
                     "node" (str saver)
                     (mapcat (juxt :address (comp str :file)) pages))]
    (with-open [said (io/reader (:out run))]
      (run! saved! (keep saved-file (line-seq said))))
    (let [{:keys [exit err]} @run]
      (when-not (zero? exit)
        (throw (ex-info (or (last-line err) "The saver stopped.") {}))))))

;; The script takes many pages in one run, and opens them all in one browser.
(defn- save-with
  "A function that saves pages with the script `saver`, each the `:address` it is at and the
   `:file` it goes into, and calls `saved!` with each file as it is saved. It throws with what
   the script said when a page could not be saved."
  [saver]
  (fn [pages saved!]
    (doseq [some-pages (partition-all pages-a-run pages)]
      (run-saver! saver some-pages saved!))))

(defn- write-text! [out file text]
  (let [path (fs/path out file)]
    (fs/create-dirs (fs/parent path))
    (spit (fs/file path) text)))

(defn- targets
  "What is saved for each of `pages` of `blog`, which is served at `origin`: the page, the
   `:address` it is at, the `:file` it goes into under `out`, and the name of that file and of
   the file of its data, from the top of the blog."
  [{:keys [root]} origin out pages]
  (for [page pages
        :let [file (output/file-of root page)]]
    {:page      page
     :address   (str origin (address/of-page root page))
     :name      file
     :file      (str (fs/path out file))
     :data-name (output/data-of root page)}))

;; The saver names a page by the address it opened, which is one of a server that is gone
;; when the author reads it. The name of the file says more.
(defn named-by-file
  "The text `message` of a saver, with the address of each of `targets` as the name of its
   file."
  [message targets]
  (reduce (fn [text {:keys [address name]}] (str/replace text address name))
          message
          (sort-by (comp - count :address) targets)))

(defn- save-pages!
  "Saves the pages `pages` of `blog`, which is served at `origin`, into `out`, says each as it
   is saved, and writes the data of each beside it, which `told` gives by the page. Gives the
   name of the file of each page."
  [{:keys [save say]} blog origin out pages told]
  (let [targets (targets blog origin out pages)
        names   (into {} (map (juxt :file :name)) targets)]
    (try
      (save targets (fn [file] (say (str "  " (names (str file))))))
      (catch Exception problem
        (throw (ex-info (str "Could not save the pages. " (named-by-file (ex-message problem) targets))
                        {}
                        problem))))
    (doseq [{:keys [page data-name]} targets
            :when data-name]
      (write-text! out data-name (pr-str (told page))))
    (mapv :name targets)))

(defn- copy-app!
  "Copies the files of the app in the directory `app` into `out`: every file but its page,
   which each page is saved from."
  [app out]
  (doseq [path  (fs/glob app "**")
          :let  [file (fs/relativize app path)]
          :when (and (fs/regular-file? path) (not= wire/index (str file)))]
    (fs/create-dirs (fs/parent (fs/path out file)))
    (fs/copy path (fs/path out file) {:replace-existing true})))

;; What is published of the files beside a text is what the text shows or links to, by the
;; one rule of what such a file is. A file that it does not name is not copied, and neither
;; is one that a page could not name, so that none takes the place of a page or of its data.
(defn- copy-media!
  "Copies the files that each of the texts `texts` of `blog` shows or links to, from beside
   it into `out`. Each text is as a page shows it."
  [blog out texts]
  (doseq [{:keys [folder] :as shown} texts
          :let                       [from (source/find-media-dir blog folder)]
          file-name                  (sort (source/files-of blog shown))
          :when                      (fs/regular-file? (fs/path from file-name))]
    (fs/create-dirs (fs/path out folder))
    (fs/copy (fs/path from file-name) (fs/path out folder file-name) {:replace-existing true})))

(defn- copy-pictures!
  "Copies the pictures of `blog`, as `pictures` holds them, into the top of `out`."
  [blog out pictures]
  (doseq [src (pictures/files pictures)]
    (fs/copy (fs/path (source/pictures-dir blog) src) (fs/path out src) {:replace-existing true})))

(defn- copy-font!
  "Copies the folder of the font of `blog` into the fonts at the top of `out`."
  [blog out]
  (when-some [dir (source/find-font-dir blog)]
    (let [to (fs/path out layout/fonts-dir (fs/file-name dir))]
      (fs/create-dirs (fs/parent to))
      (fs/copy-tree dir to {:replace-existing true}))))

(defn- write-blog!
  "Writes the whole of `blog` into the directory `out`, and gives the file of each page. Its
   pages are saved from a server that runs for as long as this takes."
  [{:keys [say app] :as how} {:keys [root] :as blog} out]
  (let [site   (source/read-site blog)
        pages  (output/pages site)
        told   (into {} (map (juxt identity #(source/read-page blog % site))) pages)
        server (http/run-server (partial serve/handler {:blog blog :app app :site site :told told})
                                {:ip "127.0.0.1" :port 0 :legacy-return-value? false})
        origin (str "http://localhost:" (http/server-port server))]
    (try
      (run! say (:problems site))
      (let [files (save-pages! how blog origin out pages told)]
        (doseq [[file text] (output/texts root site)]
          (write-text! out file text))
        (write-text! out wire/font (source/read-font-css blog))
        (copy-app! app out)
        (copy-media! blog out (distinct (keep :text (vals told))))
        (copy-pictures! blog out (:pictures site))
        (copy-font! blog out)
        files)
      (finally
        @(http/server-stop! server)))))

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

;; The pages are written beside the output directory, and take its place when every one of
;; them is there. A run that fails leaves the pages that were there as they were.
(defn generate!
  "Writes `blog` into its output directory, in place of what was there, as `how` says, and
   gives the file of each page."
  [{:keys [say] :as how} {:keys [dir] :as blog}]
  (let [docs (fs/path dir layout/output-dir)
        out  (fs/path dir (str layout/output-dir ".new"))]
    (when (fs/exists? out)
      (say (str "Removing " out ", which an earlier run left.")))
    (fs/delete-tree out)
    (try
      (let [files (write-blog! how blog out)]
        (replace! out docs)
        files)
      (finally
        (fs/delete-tree out)))))

(defn ready
  "What generating takes from BareBlog in the directory `home`: `:save` and `:app`, as
   `generate!` is given them. Or a `:refusal`, the text that says why nothing can be generated."
  [home]
  (let [saver   (find-saver home)
        app     (serve/app-dir home)
        refusal (find-refusal saver app)]
    (if refusal
      {:refusal refusal}
      {:save (save-with saver) :app app})))
