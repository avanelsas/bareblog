(ns bareblog.site
  "What every page of a blog loads: the settings of the blog and its list of posts.")

(defn entry
  "What the list of posts says of `post`, which is in the folder `folder`: enough to show it in
   a list and to link to it."
  [folder post draft?]
  (cond-> (assoc (select-keys post [:title :date :tags :summary]) :folder folder)
    draft? (assoc :draft? true)))

(defn- by-date
  "Compares two entries: the later date first, then a post with no date, then by title."
  [a b]
  (compare [(nil? (:date a)) (:date b) (:title a) (:folder a)]
           [(nil? (:date b)) (:date a) (:title b) (:folder b)]))

(defn listed
  "The `entries` in the order of the list: the latest date first, and a post with no date after
   the dated ones."
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
