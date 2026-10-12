(ns bareblog.address
  "The address of each page of a blog, and of the data that every page loads."
  (:require [bareblog.wire :as wire]
            [clojure.string :as str]))

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

(defn- asked
  "One thing that an address asks for, as it is written after the question mark."
  [[k text]]
  (str (name k) "=" (encoded text)))

(defn query
  "What is written after an address to ask for `asked-for`, a map from a name to text. A name
   with no text is left out, and nothing to ask for is no text at all."
  [asked-for]
  (let [present (filter (comp seq val) (sort-by key asked-for))]
    (if (seq present)
      (str "?" (str/join "&" (map asked present)))
      "")))

(defn post [folder]
  (str root folder "/"))

(defn tag [tag-name]
  (str root tags "/" (encoded tag-name) "/"))

(def archive
  (str root archive-name "/"))

(def site-data
  (str root wire/site-data))
