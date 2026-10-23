(ns bareblog.command.start
  "Starts a blog, and a post in one.

   The edge of the command. What a new blog and a new draft hold is pure, and is at the top."
  (:require [babashka.fs :as fs]
            [bareblog.blog.shape :as shape]
            [bareblog.blog.site :as site]
            [bareblog.blog.text :as text]
            [bareblog.blog.layout :as layout]
            [bareblog.publish.git :as git]
            [bareblog.read.source :as source]
            [bareblog.written.output :as output]
            [clojure.string :as str]))

;; What is made. Pure.

;; BareDOM names its theme presets in ClojureScript, which the command cannot load, so the
;; names are said here again. The first is the one that a reader starts with.
(def themes
  "The themes that a reader of a new blog can pick from: every theme preset of BareDOM."
  ["ocean" "default" "forest" "sunset" "neo-brutalist" "aurora" "mono-ai" "warm-mineral"])

(def ^:private example-author
  "What the author of a blog looks like in its settings, to show an author."
  "Your name")

(defn settings-text
  "The text of the settings of a new blog with the title `title`: every setting that a blog
   has a value for when it says nothing, with that value, and where its author and its address
   go, which an author gives before the blog is published."
  [title]
  (str/join "\n"
            [(str "{:title            " (pr-str (str/trim title)))
             (str " :language         " (pr-str site/default-language))
             (str " :template         " (pr-str shape/default-template))
             (str " :available-themes " (pr-str themes))
             (str " :per-page         " (pr-str site/default-per-page))
             ""
             " ;; The line \"Powered by BareBlog\" at the bottom of every page. false leaves it out."
             (str " :powered-by-footer " (pr-str site/default-powered-by-footer))
             ""
             " ;; The titles of the posts and the pages, also in a list, filled with a gradient of the"
             " ;; colours of the theme. Only true fills them."
             (str " :gradient-titles  " (pr-str site/default-gradient-titles))
             ""
             " ;; The logo in the navbar, in place of the title: an SVG, PNG, JPEG, GIF or WebP"
             " ;; beside this file. One name is the logo in both modes of the theme. A map gives one"
             " ;; for each, and the one under :dark is shown when the theme is dark."
             " ;; :logo {:light \"logo.svg\" :dark \"logo-dark.svg\"}"
             ""
             " ;; The icon of the blog in the tab of a browser: an SVG, PNG or ICO beside this file,"
             " ;; one name or a map as for the logo."
             " ;; :favicon {:light \"favicon.svg\" :dark \"favicon-dark.svg\"}"
             ""
             " ;; Set these two before you publish."
             ""
             " ;; Who writes the blog, in its feed. The title of the blog when left out."
             (str " ;; :author " (pr-str example-author))
             ""
             " ;; Where the blog is published. Every link of a published page starts there."
             (str " ;; :address " (pr-str output/example-address))
             ""
             " ;; The picture that a chat shows with a link to a page that has none of its own: a"
             " ;; PNG, JPEG or GIF beside this file. A square one of 400x400 is shown small beside"
             " ;; the text, a wide one of 1200x630 beneath it."
             " ;; :link-preview \"link-preview.jpg\""
             " }"
             ""]))

;; A blog keeps its drafts out of git, since a draft is on the disk of its author only. It
;; also keeps out the files that a Mac leaves in a folder, which a publish would commit with
;; the folder of a post.
(def ^:private drafts-line
  (str layout/drafts-dir "/"))

(def ignored
  "What a blog keeps out of git, as the lines of the file that says so."
  [drafts-line ".DS_Store" "._*"])

;; The drafts are one directory at the top, so a slash before the name or none after it keeps
;; out the same. A file of a Mac may be in any folder, and a slash before its name would keep
;; it out at the top only, which is not the same.
(def ^:private also-written
  "The other ways to write a line of `ignored` that keep out the same."
  {drafts-line #{layout/drafts-dir (str "/" layout/drafts-dir) (str "/" drafts-line)}
   ".DS_Store" #{"**/.DS_Store"}
   "._*"       #{"**/._*"}})

(defn- missing
  "The lines of `ignored` that the lines `lines` of a file of what git keeps out do not have,
   in any way they are written."
  [lines]
  (let [there (into #{} (map str/trim) lines)]
    (remove (fn [line] (some there (conj (also-written line) line))) ignored)))

(defn- line-end
  "What ends a line in the text `text`: what it uses already, or else a new line."
  [text]
  (if (str/includes? text "\r\n") "\r\n" "\n"))

(defn with-ignored
  "The text `text` of a file of what git keeps out, with each line of `ignored` that it does
   not have yet after what is there. A text that has them all is left as it is."
  [text]
  (let [end   (line-end text)
        lines (missing (str/split-lines text))
        apart (if (or (str/blank? text) (str/ends-with? text "\n")) "" end)]
    (if (empty? lines)
      text
      (str text apart (str/join end lines) end))))

(def next-steps
  "What an author does with a new blog, as text."
  (str/join "\n"
            (concat ["Next:"
                     "  bareblog new \"A first post\"    write it, in drafts"
                     "  bareblog preview                see the blog"
                     ""]
                    (output/publishing-steps git/branch))))

(defn first-text
  "What a post or a page with the title `title` starts as: its `:folder`, and the `:text` that
   it holds. A title that gives no name for a folder gives a `:problem`."
  [title]
  (let [folder (text/folder-for title)]
    (if (text/folder? folder)
      {:folder folder :text (str "# " (str/trim title) "\n\n")}
      {:problem (str "The title " (pr-str title) " gives no name for a folder. " text/folder-rule)})))

;; The making. Each function below reads or writes the disk.

(defn- empty-dir? [dir]
  (empty? (fs/list-dir dir)))

;; A blog is a repository of its own. One is made only in a directory with nothing in it, so
;; that a directory that holds other things never becomes a repository by a slip. A directory
;; inside another repository gets none, and the author is told, since a repository inside
;; another is seldom what is meant.
(defn- repository!
  "Sees to it that the directory `dir` can be a blog: it is a git repository, or is made one.
   Gives what was done, as a text under `:said`, or a `:problem`."
  [dir]
  (cond
    (git/top? dir)
    {:said "It is a git repository already."}

    (git/inside? dir)
    {:said (str "It is inside another git repository, so none was made. A blog is a repository of its "
                "own: move it out, or run git init here.")}

    (not (empty-dir? dir))
    {:problem (str dir " holds other things, and is no git repository. Make the blog in a new "
                   "directory, or run git init here first.")}

    :else
    (let [{:keys [problem]} (git/init! dir)]
      (if problem
        {:problem problem}
        {:said "It is a git repository now."}))))

(defn- write-blog!
  "Writes the settings of a blog with the title `title` in `dir`, its folders, and what git
   keeps out."
  [dir title]
  (let [ignore (fs/file dir ".gitignore")]
    (spit (fs/file dir layout/settings-file) (settings-text title))
    (run! #(fs/create-dirs (fs/path dir %)) layout/written-dirs)
    (spit ignore (with-ignored (if (fs/exists? ignore) (slurp ignore) "")))))

(defn blog!
  "Makes the directory `dir` a blog with the title `title`: a git repository, with its
   settings, its folders, and what a blog keeps out of git. Gives what it did about the
   repository, under `:said`. Gives a `:problem`, and writes nothing, when the directory is a
   blog already or cannot be a repository."
  [dir title]
  (if (source/blog? dir)
    {:problem (str "There is a " layout/settings-file " in " dir " already.")}
    (let [made (repository! dir)]
      (when-not (:problem made)
        (write-blog! dir title))
      made)))

(defn- write-text!
  "Writes `text` as the text of `folder` in the directory `in` of the blog in `dir`."
  [dir in folder text]
  (let [file (fs/path dir in folder (layout/text-file in))]
    (fs/create-dirs (fs/parent file))
    (spit (fs/file file) text)
    {:file (str file)}))

(defn- start-in!
  "Makes a folder for `title` in the directory `in` of the blog in `dir`, with its text. Gives
   the file of the text, or a `:problem` when the title gives no folder or the folder is there
   already, as a post, a draft or a page."
  [dir in title]
  (let [{:keys [folder text problem]} (first-text title)]
    (if problem
      {:problem problem}
      (if-some [taken (source/find-dir (source/preview dir) folder)]
        {:problem (str "There is a " folder " already, in " taken ".")}
        (write-text! dir in folder text)))))

(defn draft!
  "Makes the draft of a post for `title` in the blog in the directory `dir`."
  [dir title]
  (start-in! dir layout/drafts-dir title))

(defn page!
  "Makes a page that is no post for `title` in the blog in the directory `dir`."
  [dir title]
  (start-in! dir layout/pages-dir title))
