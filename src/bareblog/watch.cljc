(ns bareblog.watch
  "What a change of the files of a blog means for the pages that are open.

   A stamp is a map from the path of each file of a blog, with a slash between its names, to
   the time the file was last changed."
  (:require [bareblog.layout :as layout]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn- changed-paths
  "The paths that `before` and `now` do not hold with the same time: changed, new, and gone."
  [before now]
  (into #{}
        (remove (fn [path] (= (before path) (now path))))
        (set/union (set (keys before)) (set (keys now)))))

(defn- folder-of
  "The name of the post folder that the file at `path` is in, or nil when it is in none."
  [path]
  (let [[dir folder & more] (str/split path #"/")]
    (when (and (some #{dir} layout/post-dirs) (seq more))
      folder)))

(defn changes
  "What differs between the stamps `before` and `now`: the post folders with a changed file,
   and whether the settings of the blog changed."
  [before now]
  (let [paths (changed-paths before now)]
    {:folders   (into #{} (keep folder-of) paths)
     :settings? (contains? paths layout/settings-file)}))

(defn to-tell
  "The folders, of those in the set `listening`, whose pages `changes` makes out of date. A
   change of the settings does that to every page."
  [{:keys [folders settings?]} listening]
  (if settings?
    listening
    (set/intersection folders listening)))

(defn- with-listener [by-folder listener folder]
  (update by-folder folder (fnil conj []) listener))

(defn by-folder
  "The listeners of each folder, from `listeners`, which maps a listener to its folder."
  [listeners]
  (reduce-kv with-listener {} listeners))
