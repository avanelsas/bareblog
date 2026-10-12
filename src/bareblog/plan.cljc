(ns bareblog.plan
  "What a publish does, as a value.

   A plan says what is read and what is kept: under `:read` the post folders that are read from
   disk, under `:as-published` those that are taken as they were published last, and under
   `:dates` the date of each post whose text gives none. It says what goes: under `:removed`
   the published posts that are taken out, which the author is asked about first, and under
   `:move` the draft that becomes a post. And it says what is committed: the `:paths`, from the
   top of the blog, and the `:message`. A plan that does nothing has a `:refuse`, the text that
   says why."
  (:require [bareblog.layout :as layout]
            [bareblog.post :as post]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn- folders
  "The folders of the posts in the published site `published`, as a set."
  [published]
  (into #{} (map :folder) (:posts published)))

(defn- post-path [folder]
  (str layout/posts-dir "/" folder))

(defn published-dates
  "The date that each post in the published site `published` was published with."
  [published]
  (into {} (keep (fn [{:keys [folder date]}] (when date [folder date]))) (:posts published)))

(defn- dates
  "The date of each post: the one it was published with, and `today` for a post in `read` that
   was not published before. The text of a post that gives a date goes before these."
  [published read today]
  (merge (zipmap read (repeat today)) (published-dates published)))

(defn kind
  "What publishing a folder is, from where the folder is and whether the published list has
   it: `:edit`, `:new`, `:remove`, or `:unknown` when there is nothing of that name."
  [{:keys [listed? in-posts? in-drafts?]}]
  (cond
    (and listed? in-posts?)   :edit
    listed?                   :remove
    (or in-posts? in-drafts?) :new
    :else                     :unknown))

(defn- refused
  "Why the post in `folder` cannot be published, when it is of the kind `what`, or nil."
  [folder what]
  (cond
    (not (post/folder? folder))
    (str (pr-str folder) " is no name of a post folder. " post/folder-rule)

    (= :unknown what)
    (str "There is no post " folder ", in " layout/posts-dir " or in " layout/drafts-dir
         ", and none was published.")))

(defn one
  "The plan for publishing the post in `folder`, for the site `published` as it was published
   last, which is nil for a blog that never was. `where` says where the folder is, with
   `:in-posts?` and `:in-drafts?`. A new post gets `today` as its date."
  [published folder where today]
  (let [listed (folders published)
        what   (kind (assoc where :listed? (contains? listed folder)))
        read   (if (= :remove what) #{} #{folder})]
    (if-some [why (refused folder what)]
      {:refuse why}
      (cond-> {:read         read
               :as-published (disj listed folder)
               :dates        (dates published read today)
               :paths        [(post-path folder) layout/output-dir]
               :message      (str (if (= :remove what) "Remove " "Publish ") folder)}
        (= :remove what)                             (assoc :removed #{folder})
        (and (= :new what) (not (:in-posts? where))) (assoc :move folder)))))

;; An edit that the author keeps back stays out of the pages and out of the commit: the post
;; is taken as it was published, and its folder is not among the paths.
(defn all
  "The plan for publishing the whole blog, for the site `published` as it was published last.
   `on-disk` is the set of post folders that there are, and `held-back` the set of those whose
   edits the author keeps back. A post that was not published before gets `today` as its
   date."
  [published on-disk held-back today]
  (let [read    (set/difference on-disk held-back)
        removed (set/difference (folders published) on-disk)]
    (cond-> {:read         read
             :as-published (set held-back)
             :dates        (dates published read today)
             :paths        (into [layout/settings-file layout/output-dir]
                                 (map post-path (sort (set/union read removed))))
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
