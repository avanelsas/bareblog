(ns bareblog.watch
  "What a change of the files of a blog means for the pages that are open.

   A stamp is a map from the path of each file of a blog, with a slash between its names, to
   the time the file was last changed. Every page shows something of the whole blog, so any
   difference between two stamps makes every open page out of date.")

(defn changed?
  "True when the stamps `before` and `now` differ: a file changed, is new, or is gone."
  [before now]
  (not= before now))

(defn- with-listener [by-page listener page]
  (update by-page page (fnil conj []) listener))

(defn by-page
  "The listeners of each page, from `listeners`, which maps a listener to its page."
  [listeners]
  (reduce-kv with-listener {} listeners))
