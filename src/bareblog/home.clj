(ns bareblog.home
  "Where the things of BareBlog itself are, from its directory."
  (:require [babashka.fs :as fs]))

(defn app-dir
  "The directory of the files of the app, for BareBlog in the directory `home`."
  [home]
  (fs/path home "resources" "public"))
