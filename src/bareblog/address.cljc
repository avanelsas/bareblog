(ns bareblog.address
  "The address of each page of a blog, and of the data that every page loads."
  (:require [bareblog.wire :as wire]
            #?(:clj [clojure.string :as str])))

(def root "/")

(def pages "page")
(def tags "tags")
(def archive-name "archive")

(def reserved
  "The names at the top of a blog that are not the name of a post folder."
  #{pages tags archive-name "js"})

(defn encoded
  "The text `text` as one name of an address."
  [text]
  #?(:clj  (str/replace (java.net.URLEncoder/encode ^String text "UTF-8") "+" "%20")
     :cljs (js/encodeURIComponent text)))

(defn home
  "The address of page `number` of the home page. The first is the top of the blog."
  [number]
  (if (= 1 number)
    root
    (str root pages "/" number "/")))

(defn post [folder]
  (str root folder "/"))

(defn tag [tag-name]
  (str root tags "/" (encoded tag-name) "/"))

(def archive
  (str root archive-name "/"))

(def site-data
  (str root wire/site-data))
