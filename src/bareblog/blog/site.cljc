(ns bareblog.blog.site
  "What every page of a blog loads: the settings of the blog and its list of posts."
  (:require [bareblog.blog.layout :as layout]
            [bareblog.blog.text :as text]))

(defn entry
  "What the list of posts says of `post`, which is in the folder `folder`: enough to show it in
   a list, with the address of its first picture, and to link to it."
  [folder post draft?]
  (let [picture (:src (first (text/pictures post)))]
    (cond-> (assoc (select-keys post [:title :date :tags :summary]) :folder folder)
      picture (assoc :picture picture)
      draft?  (assoc :draft? true))))

;; The date of a post is the moment it was first published, on the clock of its author:
;; `2026-10-12T14:03:22+02:00`. It is compared as text, which is the order of time for as long
;; as that clock is not set back. A post that was never published has none. It goes first,
;; which is where it is once it is published.
(defn day
  "The day of the date `date`, as `2026-10-12`, or nil when there is no date."
  [date]
  (some-> date (subs 0 10)))

(defn post-days
  "The days on which the posts of `site` were first published, oldest first, each once."
  [site]
  (vec (into (sorted-set) (keep (comp day :date)) (:posts site))))

(defn- by-date
  "Compares two entries: a post with no date first, then the later date, then by title."
  [a b]
  (compare [(some? (:date a)) (:date b) (:title a) (:folder a)]
           [(some? (:date b)) (:date a) (:title b) (:folder b)]))

(defn listed
  "The `entries` in the order of the list: a post with no date first, and then the latest
   date."
  [entries]
  (vec (sort by-date entries)))

(defn tags
  "Every tag of the posts in `site`, once, in the order of the alphabet."
  [site]
  (sort (distinct (mapcat :tags (:posts site)))))

(defn with-problems
  "The `site` with the texts `problems` after what is wrong with it already. A site with
   nothing wrong says nothing of problems."
  [site problems]
  (let [all (into (vec (:problems site)) problems)]
    (cond-> site
      (seq all) (assoc :problems all))))

(def default-powered-by-footer
  "What a new blog writes in its settings for the line \"Powered by BareBlog\", which is what a
   blog that says nothing of it gets."
  true)

;; Only false leaves the line out, so that a value written wrong does not hide it.
(defn powered-by-footer?
  "True when the pages of the blog with the settings `settings` end with the line \"Powered by
   BareBlog\"."
  [settings]
  (not (false? (:powered-by-footer settings))))

(defn settings-problems
  "What is wrong with the settings `settings`, as texts."
  [settings]
  (when-some [value (:powered-by-footer settings)]
    (when-not (boolean? value)
      [(str layout/settings-file " gives " (pr-str value) " as its :powered-by-footer, which is "
            "true or false.")])))

(defn site
  "The data that every page loads, from the settings of the blog and the entries of its posts.
   `problems` are texts that say what is wrong with the blog."
  [settings entries problems]
  (with-problems {:blog settings :posts (listed entries)} problems))

;; What is wrong with a blog is for its author. A reader is not told.
(defn published
  "The `site` as the readers of a published blog load it: with nothing of what is wrong."
  [site]
  (dissoc site :problems))

(def default-language
  "The language of a blog that names none."
  "en")

(defn dates
  "The date of each post and page in `site` that has one, by its folder."
  [site]
  (into {}
        (keep (fn [{:keys [folder date]}] (when date [folder date])))
        (concat (:posts site) (:pages site))))

(def default-per-page 10)

(defn per-page
  "How many posts a page of a list of entries shows."
  [site]
  (get-in site [:blog :per-page] default-per-page))

(defn page-count
  "How many pages the posts of `site` fill as a list of entries. An empty list fills one."
  [site]
  (let [per-page (per-page site)]
    (max 1 (quot (+ (count (:posts site)) (dec per-page)) per-page))))

(defn page-entry
  "What the list of pages says of `page`, a page of the blog that is no post, in the folder
   `folder`: enough to link to it."
  [folder page]
  (assoc (select-keys page [:title :date :summary]) :folder folder))

;; A page that was never published comes last, which is where it is once it is published.
(defn- by-first-publish
  "Compares two entries of pages: the one that was published first goes first, then a page
   that was not published, then by title."
  [a b]
  (compare [(nil? (:date a)) (or (:date a) "") (:title a) (:folder a)]
           [(nil? (:date b)) (or (:date b) "") (:title b) (:folder b)]))

(defn with-pages
  "The `site` with the entries `pages` of its pages that are no posts, in the order of their
   links. A site with none says nothing of pages."
  [site pages]
  (cond-> site
    (seq pages) (assoc :pages (vec (sort by-first-publish pages)))))

(defn newest-folder
  "The name of the folder of the newest post in `site`, or nil when it has no posts."
  [site]
  (:folder (first (:posts site))))

(defn told
  "What a page is told to show, from the data of the site and of the page: both, or the problem
   that one of them is."
  [site page]
  (or (first (filter :problem [site page]))
      {:site site :page page}))

(defn notice
  "One text for what is wrong with the blog in `site`: the first problem, and how many more
   there are. Nil when nothing is wrong."
  [{:keys [problems]}]
  (when-some [[problem & more] (seq problems)]
    (if (seq more)
      (str problem " There are " (count more) " more in the terminal.")
      problem)))
