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

(defn site
  "The data that every page loads, from the settings of the blog and the entries of its posts.
   `problems` are texts that say what is wrong with the blog. They are left out when there are
   none."
  [settings entries problems]
  (cond-> {:blog  settings
           :posts (listed entries)}
    (seq problems) (assoc :problems (vec problems))))

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
