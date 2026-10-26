(ns bareblog.blog.series
  "The series of a blog: posts that are read in an order, under a name.

   The series of a blog are a map from the name of each series to the folders of its parts, in
   order. `series.edn` holds them, beside the settings, and only `bareblog series` writes it. A
   post is in one series at most. A part that is a draft is in the series once it is
   published."
  (:require [clojure.string :as str]))

(defn- entry?
  "True when `entry` is a series: a name, and the folders of its parts."
  [[series-name parts]]
  (and (string? series-name) (vector? parts) (every? string? parts)))

(defn well-formed?
  "True when `series` is a map from a name to the folders of its parts."
  [series]
  (and (map? series) (every? entry? series)))

(defn- parts-in
  "The `parts` of a series that are among the `folders`, in order."
  [folders parts]
  (filterv (set folders) parts))

(defn- kept-parts
  "The series `series-name` with only its `parts` among the `folders`, or nil when none is."
  [folders [series-name parts]]
  (when-some [kept (not-empty (parts-in folders parts))]
    [series-name kept]))

(defn shown
  "The `series` with only their parts among the `posts`, and without a series that has none of
   them."
  [series posts]
  (into {} (keep (partial kept-parts (map :folder posts))) series))

(defn by-folder
  "The `posts` by their folder."
  [posts]
  (into {} (map (juxt :folder identity)) posts))

(defn- place-in
  "The series `series-name` and the place of `folder` among its `parts` from 0, or nil when
   it is none of them."
  [folder [series-name parts]]
  (let [i (.indexOf parts folder)]
    (when-not (neg? i)
      [series-name i])))

(defn- found-in
  "The name of the series among `series` that the post in `folder` is a part of, and its place
   there from 0, or nil."
  [series folder]
  (some (partial place-in folder) (sort-by key series)))

(defn series-of
  "The name of the series among `series` that the post in `folder` is a part of, or nil."
  [series folder]
  (first (found-in series folder)))

(defn of-post
  "The series among `series`, as `shown` gives them for the `posts`, that the post in `folder`
   is a part of: its `:name`, its `:parts` as their entries among the `posts`, the `:number` of
   the post among them from 1, and the `:next` part, which the last has none of. Nil when the
   post is in none."
  [series posts folder]
  (when-some [[series-name place] (found-in series folder)]
    (let [parts (mapv (by-folder posts) (get series series-name))]
      {:name   series-name
       :parts  parts
       :number (inc place)
       :next   (get parts (inc place))})))

(defn command
  "The command that makes the series `series-name` of the parts in the folders `parts`."
  [series-name parts]
  (str/join " " (into ["bareblog series --name" (pr-str series-name)] parts)))

(defn- gone-problems
  "Each series of `series` that names a folder that is not among the `folders`, as a text with
   the command that keeps it without them."
  [series folders]
  (for [[series-name parts] (sort-by key series)
        :let  [gone (remove (set folders) parts)]
        :when (seq gone)]
    (str "The series " (pr-str series-name) " names " (str/join " and " gone)
         (if (next gone) ", which are no posts or drafts" ", which is no post or draft")
         ". Keep the series without " (if (next gone) "them" "it") " with: "
         (command series-name (parts-in folders parts))
         ". A post that was renamed goes in its place, by its new folder or title.")))

(defn- names-by-part
  "The names of the series of `series` that each folder is a part of, by the folder."
  [series]
  (apply merge-with into (for [[series-name parts] (sort-by key series)
                               part                (distinct parts)]
                           {part [series-name]})))

(defn- shared-problems
  "Each post that more than one series of `series` names, as a text with the command that takes
   it out of the last of them."
  [series]
  (for [[part names] (sort-by key (names-by-part series))
        :when        (next names)
        :let         [last-name (peek names)]]
    (str part " is in the series " (str/join " and " (map pr-str names))
         ", and a post is in one series. Take it out of " (pr-str last-name) " with: "
         (command last-name (remove #{part} (get series last-name))))))

(defn problems
  "What is wrong with the `series` of a blog whose posts and drafts are in the `folders`, as
   texts: each series that names a folder that is neither, and each post in more than one
   series, with the command that mends it."
  [series folders]
  (concat (gone-problems series folders) (shared-problems series)))

(defn text
  "The `series` as `series.edn` holds them: a series on each line, by name."
  [series]
  (if (empty? series)
    "{}\n"
    (str "{" (str/join "\n " (for [[series-name parts] (sort-by key series)]
                               (str (pr-str series-name) " " (pr-str parts))))
         "}\n")))
