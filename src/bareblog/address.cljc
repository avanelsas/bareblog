(ns bareblog.address
  "The address of each page of a blog, and of the data that every page loads.

   An address starts at the top of the blog, which is `top` in the preview and the path of the
   address in the settings when the blog is generated. Every function here takes it as `root`."
  (:require [bareblog.wire :as wire]
            [clojure.string :as str]))

(def top
  "The top of a blog that is served at the top of its host."
  "/")

(def pages "page")
(def tags "tags")
(def archive-name "archive")

(def reserved
  "The names at the top of a blog that are not the name of a post folder."
  #{pages tags archive-name "js"})

(def ^:private written
  "How the address in the settings of a blog is written: its scheme and host, the host alone,
   and the path after it."
  #"(https?://([^/?#]+))(/[^?#]*)?")

(defn- with-slash [path]
  (if (str/ends-with? path "/") path (str path "/")))

(defn of-blog
  "What the address `address` in the settings of a blog says: its `:origin`, which is its
   scheme and host, its `:host`, and its `:root`, which is its path with a slash at the end.
   A blog that names no address has no host, and is at the top of whatever host sends it."
  [address]
  (let [[_ origin host path] (re-matches written (or address ""))]
    {:origin (or origin "")
     :host   host
     :root   (if path (with-slash path) top)}))

(defn encoded
  "The text `text` as one name of an address."
  [text]
  #?(:clj  (str/replace (java.net.URLEncoder/encode ^String text "UTF-8") "+" "%20")
     :cljs (js/encodeURIComponent text)))

(defn decoded
  "The address `text` with each percent escape as the character it stands for, or nil when an
   escape is broken."
  [text]
  #?(:clj  (try (java.net.URLDecoder/decode (str/replace text "+" "%2B") "UTF-8")
                (catch IllegalArgumentException _ nil))
     :cljs (try (js/decodeURIComponent text)
                (catch :default _ nil))))

(defn home
  "The address of page `number` of the home page. The first is the top of the blog."
  [root number]
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

(defn post [root folder]
  (str root folder "/"))

(defn tag [root tag-name]
  (str root tags "/" (encoded tag-name) "/"))

(defn archive [root]
  (str root archive-name "/"))

(defn file
  "The address of the file `file-name` at the top of the blog."
  [root file-name]
  (str root file-name))

(defn site-data [root]
  (file root wire/site-data))

(defn feed [root]
  (file root wire/feed))

(defn- of-home [root {:keys [number]}]
  (home root number))

(defn- of-tag [root page]
  (tag root (:tag page)))

(defn- of-archive [root _page]
  (archive root))

(defn- of-post [root {:keys [folder]}]
  (post root folder))

(defn- of-missing [root _page]
  (file root wire/missing))

(def ^:private of-kind
  "What gives the address of each kind of page."
  {:home      of-home
   :tag       of-tag
   :archive   of-archive
   :post      of-post
   :not-found of-missing})

(defn of-page
  "The address of the page `page`, which is a map with its `:kind`."
  [root page]
  ((of-kind (:kind page)) root page))
