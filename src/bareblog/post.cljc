(ns bareblog.post
  "A post, the name of its folder, and the files beside it."
  (:require [bareblog.wire :as wire]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(def folder-pattern
  "What the name of a post folder looks like, as the text of a pattern. The name is the address
   of the post."
  "[a-z0-9][a-z0-9-]*")

(defn from
  "The post in the folder `folder`, from what the reader read of its text. Its title is the one
   in its header block, or else the heading that opens it, or else the name of its folder."
  [folder {:keys [fields heading blocks]}]
  (assoc fields
         :title  (or (:title fields) heading folder)
         :blocks blocks))

(def file-pattern
  "What the name of a file beside a post looks like, as the text of a pattern."
  "(?!\\.)[^/\\\\:]+")

(defn decoded
  "The address `text` with each percent escape as the character it stands for, or nil when an
   escape is broken."
  [text]
  #?(:clj  (try (java.net.URLDecoder/decode (str/replace text "+" "%2B") "UTF-8")
                (catch IllegalArgumentException _ nil))
     :cljs (try (js/decodeURIComponent text)
                (catch :default _ nil))))

(defn file-beside
  "The name of the file beside a post that the address `source` names, or nil when it names
   none."
  [source]
  (when-some [file-name (some-> source decoded)]
    (when (and (not (str/includes? file-name ".."))
               (not (contains? wire/reserved file-name))
               (re-matches (re-pattern file-pattern) file-name))
      file-name)))

(defn- shows-file?
  "True when `x` is a thing that shows a file: it has a `:src`."
  [x]
  (and (map? x) (contains? x :src)))

(defn sources
  "The addresses in `post` that name a file beside it, as a set."
  [post]
  (into #{}
        (comp (filter shows-file?) (map :src) (filter file-beside))
        (tree-seq coll? seq (:blocks post))))

(defn- with-size [sizes x]
  (if (shows-file? x)
    (merge x (sizes (:src x)))
    x))

(defn sized
  "The `post` with the width and the height of each thing that shows a file, where `sizes` has
   them under its address."
  [post sizes]
  (update post :blocks (partial walk/postwalk (partial with-size sizes))))
