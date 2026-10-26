(ns bareblog.command.series
  "The `bareblog series` command: the series of a blog, and a series made, changed or removed.

   The edge of the command. What a series is, is `bareblog.blog.series`, which is pure, and so
   is `plan`. Each function here is given how, with `:say`, and `:ask` that puts a question to
   the author and gives true for a yes. It gives the number that the process ends with."
  (:require [babashka.fs :as fs]
            [bareblog.blog.layout :as layout]
            [bareblog.blog.series :as series]
            [bareblog.blog.text :as text]
            [bareblog.read.reader :as reader]
            [bareblog.read.source :as source]
            [clojure.string :as str]))

;; What the command does. Pure.

(defn- same-title? [a b]
  (= (str/lower-case (str/trim a)) (str/lower-case (str/trim b))))

(defn- matched
  "The post among `posts` that `named` names, by its folder or its title, as `:folder`, or the
   `:problem` when none does or more than one does."
  [posts named]
  (let [by-title (filter (comp (partial same-title? named) :title) posts)]
    (cond
      (some (comp #{named} :folder) posts) {:folder named}
      (= 1 (count by-title))               {:folder (:folder (first by-title))}
      (seq by-title)                       {:problem (str (pr-str named) " is the title of "
                                                          (str/join " and " (map :folder by-title))
                                                          ". Name the one you mean by its folder.")}
      :else                                {:problem (str "No post or draft has the title or the folder "
                                                          (pr-str named) ".")})))

(defn- twice
  "The folders that are among `folders` more than once."
  [folders]
  (for [[folder n] (frequencies folders) :when (< 1 n)] folder))

(defn- elsewhere
  "Why the folders `folders` cannot be the parts of the series `series-name` among `all`: each
   that another series has, as texts."
  [all series-name folders]
  (let [others (dissoc all series-name)]
    (for [folder folders
          :let   [other (series/series-of others folder)]
          :when  other]
      (str folder " is a part of the series " (pr-str other) ", and a post is in one series."))))

(defn plan
  "What `bareblog series --name series-name named...` does to the series `all`, for the posts
   and drafts `posts`, each with its folder and title: why it is refused under `:refusal`, that
   it changes nothing under `:unchanged?`, or the series it becomes under `:series`, with the
   parts of the series named `:before` and `:after`."
  [all posts series-name named]
  (let [before  (get all series-name)
        matches (map (partial matched posts) named)
        after   (mapv :folder matches)
        doubled (twice after)
        taken   (elsewhere all series-name after)]
    (cond
      (str/blank? series-name)
      {:refusal "Give the series a name: bareblog series --name \"A series\" \"A post\" \"Another post\""}

      (and (empty? named) (nil? before))
      {:refusal (str "There is no series " (pr-str series-name) ". bareblog series lists them.")}

      (empty? named)
      {:series (dissoc all series-name) :before before :after []}

      (some :problem matches)
      {:refusal (str/join "\n" (keep :problem matches))}

      (seq doubled)
      {:refusal (str "The series names " (str/join ", " doubled) " more than once.")}

      (seq taken)
      {:refusal (str/join "\n" taken)}

      (= before after)
      {:unchanged? true}

      :else
      {:series (assoc all series-name after) :before before :after after})))

(defn- part-line
  "The part in `folder`, the `i`th of its series from 0, as a line, with its title among the
   posts `by-folder` and whether it is a draft or is gone."
  [by-folder i folder]
  (let [{:keys [title draft?] :as post} (by-folder folder)]
    (str "  " (inc i) ". " (if post
                             (str title " (" folder (when draft? ", a draft") ")")
                             (str folder " (gone)")))))

(defn- part-lines [posts parts]
  (map-indexed (partial part-line (series/by-folder posts)) parts))

;; Doing it.

(defn- read-post
  "The folder, the title and whether it is a draft, of the text `found`."
  [{:keys [folder file draft?]}]
  {:folder folder
   :title  (:title (text/from folder (reader/read-post (slurp file))))
   :draft? draft?})

(defn- read-posts
  "Each post and draft of the blog in the directory `dir`, with its folder, its title and
   whether it is a draft."
  [dir]
  (map read-post (remove :page? (source/list-texts dir))))

(defn list!
  "Says each series of the blog in the directory `dir`, with its parts in order, and what is
   wrong with them."
  [{:keys [say]} dir]
  (let [all   (source/read-series dir)
        posts (read-posts dir)]
    (if (empty? all)
      (say "There is no series. Make one with: bareblog series --name \"A series\" \"A post\" \"Another post\"")
      (do (doseq [[series-name parts] (sort-by key all)]
            (say series-name)
            (run! say (part-lines posts parts)))
          (run! say (series/problems all (map :folder posts)))))
    0))

(defn- written!
  "Writes `all` as the series of the blog in the directory `dir`, and says what came of it."
  [{:keys [say]} dir all done]
  (spit (fs/file dir layout/series-file) (series/text all))
  (say (str done " It is published with the next publish of any post."))
  0)

(defn- show!
  "Says what the series `series-name` is and becomes, for the posts `posts`."
  [{:keys [say]} posts series-name before after]
  (cond
    (empty? after) (say (str "The series " (pr-str series-name) ":"))
    (nil? before)  (say (str "A new series " (pr-str series-name) ":"))
    :else          (do (say (str "The series " (pr-str series-name) " is now:"))
                       (run! say (part-lines posts before))
                       (say "It becomes:")))
  (run! say (part-lines posts (if (empty? after) before after))))

(defn make!
  "Makes, changes or removes the series `series-name` of the blog in the directory `dir`, with
   the posts and drafts that `named` names in order, once the author says yes. With nothing
   named, the series is removed, and its posts stay."
  [{:keys [say ask] :as how} dir series-name named]
  (let [posts   (read-posts dir)
        planned (plan (source/read-series dir) posts series-name named)
        quoted  (pr-str series-name)
        {:keys [refusal unchanged? series before after]} planned]
    (cond
      refusal    (do (say refusal) 1)
      unchanged? (do (say (str "The series " quoted " is like that already.")) 0)
      :else
      (let [[question done] (if (empty? after)
                              [(str "Remove the series " quoted "? Its posts stay.")
                               (str "The series " quoted " is removed, and its posts stay.")]
                              [(str (if before "Change" "Make") " the series " quoted "?")
                               (str "The series " quoted " is written in " layout/series-file ".")])]
        (show! how posts series-name before after)
        (if (ask question)
          (written! how dir series done)
          (do (say "Nothing was changed.") 1))))))
