(ns bareblog.blog.tags
  "The tags of a blog, as the one tag that each is in any spelling: which tags are written in
   more than one way, and whether one can be renamed as another."
  (:require [clojure.string :as str]))

;; A tag is one tag in any case and with a dash, an underscore or a space between its words.
;; Two spellings that differ in case get directories that a file system which ignores case
;; takes as one. Those that differ in a dash or a space are held to one spelling as well.
(defn folded
  "The tag `tag-name` as it is in any spelling."
  [tag-name]
  (-> tag-name str/lower-case (str/replace #"[\s_-]+" "-")))

(defn- folders-by-tag
  "The folders of the `posts` that have each tag, by the tag."
  [posts]
  (apply merge-with into (for [{:keys [folder tags]} posts
                               tag-name              tags]
                           {tag-name [folder]})))

(defn collisions
  "The tags of `posts` that are written in more than one way, each as a sorted map from each
   spelling to the folders of the posts that have it."
  [posts]
  (->> (folders-by-tag posts)
       (group-by (comp folded key))
       vals
       (filter next)
       (map (partial into (sorted-map)))
       (sort-by (comp key first))))

(defn- by-use
  "Compares two spellings with their folders: the one of more posts first, then by the
   alphabet."
  [[a a-folders] [b b-folders]]
  (compare [(count b-folders) a] [(count a-folders) b]))

(defn preferred
  "The spelling among `spellings` that the others are renamed as: the one of most posts."
  [spellings]
  (key (first (sort by-use spellings))))

(defn rename-command
  "The command that renames the tag `from` as `to` in every post."
  [from to]
  (str "bareblog tags rename " (pr-str from) " " (pr-str to)))

(defn- said
  "A spelling with the folders of its posts, which are as they were published when they are
   all in `published`."
  [published [spelling folders]]
  (str (pr-str spelling) " in " (str/join ", " folders)
       (when (every? published folders) " (as it was published)")))

;; A post that is taken as it was published keeps its tags until it is published again, also
;; when a rename changed them on disk. Where every post of the tag is such a post, a rename may
;; be done already, so none is named.
(defn remedy
  "What writes `spellings` one way, where the posts in `published` are as they were published:
   the renames, as `[from to]`, and whether to publish the posts together."
  [published spellings]
  (let [to      (preferred spellings)
        folders (apply concat (vals spellings))]
    (if (every? published folders)
      {:renames [] :publish? true}
      {:renames  (vec (for [from (keys spellings) :when (not= to from)] [from to]))
       :publish? (boolean (some published folders))})))

(def ^:private publish-all
  "publish them together with: bareblog publish --all")

(defn- remedy-text
  "The remedy `remedy` as the author is told it."
  [{:keys [renames publish?]}]
  (if (empty? renames)
    (str "Once they are written one way on disk, which bareblog tags shows, " publish-all)
    (str "Write them one way with: " (str/join " and " (map (partial apply rename-command) renames))
         (when publish? (str ", and " publish-all)))))

(defn problems
  "What is wrong with the tags of `posts`, as texts: each tag that is written in more than one
   way. `published` holds the folders of the posts that are as they were published."
  [posts published]
  (for [spellings (collisions posts)]
    (str "The tags " (str/join " and " (map (partial said published) spellings))
         " are one tag written in different ways, and cannot each have a page. "
         (remedy-text (remedy published spellings)))))

(defn rename-refusal
  "Why the tag `from` cannot be renamed as `to` among the tags `tags`, or nil when it can.
   A rename to another spelling of the same tag can always be done."
  [tags from to]
  (let [others (->> tags
                    (filter (comp #{(folded to)} folded))
                    (remove (set [from to]))
                    distinct)]
    (cond
      (not (some #{from} tags))
      (str "No post or page has the tag " (pr-str from) ".")

      (= from to)
      (str "The tag is " (pr-str to) " already.")

      (and (seq others) (not= (folded from) (folded to)))
      (str (pr-str to) " is one tag with " (str/join " and " (map pr-str others))
           ", written in another way. Rename as that, or rename that as well."))))

(defn renamed
  "The tags `tags` with `from` as `to`, each once."
  [tags from to]
  (vec (distinct (replace {from to} tags))))
