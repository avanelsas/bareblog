(ns bareblog.publish.plan
  "What a publish does, as a value.

   A plan says what is read and what is kept: under `:read` the folders of the posts and the
   pages that are read from disk, under `:as-published` those that are taken as they were
   published last, and under `:dates` the date of each post and page, which is the moment it
   was first published. Under `:link-preview-in` it names the directory, from the top of the
   blog, that the link preview is taken from, where that is not the top. It says what goes:
   under `:removed` the published posts that are taken out, which the author is asked about
   first, and under `:move` the draft that becomes a post. And it says what is committed: the
   `:paths`, from the top of the blog, and the `:message`. A plan that does nothing has a
   `:refuse`, the text that says why."
  (:require [bareblog.blog.site :as site]
            [bareblog.blog.text :as text]
            [bareblog.blog.layout :as layout]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn- folders
  "The folders of the entries under `k`, `:posts` or `:pages`, in the published site
   `published`, as a set."
  [published k]
  (into #{} (map :folder) (get published k)))

(defn- path
  "The directory of the folder `folder`, from the top of the blog: among the pages when it is
   one, and among the posts when it is not."
  [page? folder]
  (str (if page? layout/pages-dir layout/posts-dir) "/" folder))

(defn- dates
  "The date of each post and page: the one it was published with, and `now` for one in `read`
   that was not published before."
  [published read now]
  (merge (zipmap read (repeat now)) (site/dates published)))

;; Where a folder is says what it is: a post among the posts or the drafts, and a page among
;; the pages. A post goes before a page of the same name. The published list says what a
;; folder was only when the folder is gone.
(defn page?
  "True when the folder that `where` describes is a page of the blog that is no post."
  [{:keys [listed-page? in-posts? in-drafts? in-pages?]}]
  (boolean (cond
             (or in-posts? in-drafts?) false
             in-pages?                 true
             :else                     listed-page?)))

;; A folder that was published as the one and is now the other has two places: where it is,
;; and where it was. Both are committed, so that the place it left goes too.
(defn- paths
  "The directories of the folder `folder` to commit, from the top of the blog."
  [{:keys [listed-page? listed-post?] :as where} folder]
  (let [page? (page? where)]
    (cond-> [(path page? folder)]
      (and page? listed-post?)       (conj (path false folder))
      (and (not page?) listed-page?) (conj (path true folder)))))

(def ^:private kinds
  "What publishing a folder is, by whether the published list has it and whether it is there
   to be read."
  {[true true]   :edit
   [true false]  :remove
   [false true]  :new
   [false false] :unknown})

;; A page is read from among the pages, and a post from among the posts. A post that the list
;; does not have may be a draft still. One that the list has is not read from the drafts, so
;; that moving a post back there takes it out.
(defn kind
  "What publishing a folder is, from where the folder is and whether the published list has
   it: `:edit`, `:new`, `:remove`, or `:unknown` when there is nothing of that name."
  [{:keys [listed-post? listed-page? in-posts? in-drafts? in-pages?] :as where}]
  (let [listed? (boolean (or listed-post? listed-page?))
        placed? (if (page? where) in-pages? in-posts?)
        there?  (boolean (if listed? placed? (or placed? in-drafts?)))]
    (kinds [listed? there?])))

(defn- refused
  "Why the post in `folder` cannot be published, when it is of the kind `what`, or nil."
  [folder what]
  (cond
    (not (text/folder? folder))
    (str (pr-str folder) " is no name of a folder. " text/folder-rule)

    (= :unknown what)
    (str "There is nothing named " folder " in " (str/join ", " layout/written-dirs)
         ", and nothing of that name was published. A folder counts when its "
         (str/join " or " (sort layout/text-files)) " is in it.")))

(defn one
  "The plan for publishing the post or the page in `:folder`, for the site `:published` as it
   was published last, which is nil for a blog that never was. `:where` says where the folder
   is, with `:in-posts?`, `:in-drafts?` and `:in-pages?`. A new post gets `:now` as its date."
  [{:keys [published folder where now]}]
  (let [posts  (folders published :posts)
        pages  (folders published :pages)
        where  (assoc where
                      :listed-post? (contains? posts folder)
                      :listed-page? (contains? pages folder))
        page?  (page? where)
        what   (kind where)
        read   (if (= :remove what) #{} #{folder})]
    (if-some [why (refused folder what)]
      {:refuse why}
      (cond-> {:read         read
               :as-published (disj (into posts pages) folder)
               :dates        (dates published read now)
               :paths        (conj (paths where folder) layout/output-dir)
               :message      (str (if (= :remove what) "Remove " "Publish ") folder)}
        published                                                  (assoc :link-preview-in layout/output-dir)
        (= :remove what)                                           (assoc :removed #{folder})
        (and (= :new what) (not page?) (not (:in-posts? where))) (assoc :move folder)))))

;; A link preview that is not there is a problem of the blog, and git commits nothing for a
;; path with nothing under it.
(defn- settings-paths
  "The paths of the settings `settings`: their file, and the link preview they name."
  [{:keys [link-preview]}]
  (cond-> [layout/settings-file]
    (text/link-preview-name? link-preview) (conj link-preview)))

;; An edit that the author keeps back stays out of the pages and out of the commit: the post
;; is taken as it was published, and its folder is not among the paths.
(defn all
  "The plan for publishing the whole blog with the settings `:settings`, for the site
   `:published` as it was published last. `:on-disk` holds the folders that there are, as a
   set under `:posts` and one under `:pages`, and `:held-back` is the set of those whose edits
   the author keeps back. A post that was not published before gets `:now` as its date."
  [{:keys [published settings on-disk held-back now]}]
  (let [listed  {:posts (folders published :posts) :pages (folders published :pages)}
        there   (into (set (:posts on-disk)) (:pages on-disk))
        read    (set/difference there held-back)
        removed (set/difference (into (:posts listed) (:pages listed)) there)
        where   (fn [folder]
                  {:in-posts?    (contains? (:posts on-disk) folder)
                   :in-pages?    (contains? (:pages on-disk) folder)
                   :listed-post? (contains? (:posts listed) folder)
                   :listed-page? (contains? (:pages listed) folder)})]
    (cond-> {:read         read
             :as-published (set held-back)
             :dates        (dates published read now)
             :paths        (into (conj (settings-paths settings) layout/output-dir)
                                 (mapcat (fn [folder] (paths (where folder) folder)))
                                 (sort (set/union read removed)))
             :message      "Publish the whole blog"}
      (seq removed) (assoc :removed removed))))

;; What the author is asked. The edge asks, with these words.

(defn- names [folders]
  (str/join ", " (sort folders)))

(defn removal-question
  "What the author is asked before the published posts in `removed` are taken out."
  [removed]
  (str "Remove from the published blog: " (names removed) "?"))

(defn edits-question
  "What the author is asked about the published posts in `edited`, whose text on disk is not
   what was published."
  [edited]
  (str "These published posts have edits that are not published: " (names edited)
       ". Publish the edits too?"))

;; What the author is told of what git keeps out. The edge tells, with these words.

(defn- described
  "What says which path git keeps out, and which line of which file does so."
  [{:keys [path pattern source]}]
  (str path ", by the line " pattern " in " source))

(defn- all-described [ignored]
  (str/join ". " (map described (sort-by :path ignored))))

(defn ignored-refusal
  "Why a publish that commits the paths in `ignored` cannot be, when git keeps them out."
  [ignored]
  (str "Git keeps out: " (all-described ignored) ". A publish commits all of this, so nothing "
       "was written. Change what keeps it out, and publish again."))

(defn ignored-notice
  "What the author is told of the files in `ignored`, which are below what a publish commits
   and are left out of it by git."
  [ignored]
  (str "Git keeps out, and so leaves out of what is published: " (all-described ignored) "."))
