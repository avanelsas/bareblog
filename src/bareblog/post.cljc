(ns bareblog.post
  "A post, the name of its folder, and the files beside it."
  (:require [bareblog.address :as address]
            [bareblog.wire :as wire]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(def folder-pattern
  "What the name of a post folder looks like, as the text of a pattern. The name is the address
   of the post."
  "[a-z0-9][a-z0-9-]*")

(defn folder?
  "True when `folder-name` can be the name of a post folder: it looks like one, and the blog
   keeps no address of that name for itself."
  [folder-name]
  (and (some? (re-matches (re-pattern folder-pattern) folder-name))
       (not (contains? address/reserved folder-name))))

(defn- plain
  "The text of `x`, which is content of a block or a block that holds content."
  [x]
  (cond
    (string? x) x
    (map? x)    (or (:text x) (apply str (map plain (:content x))))
    :else       ""))

(def summary-length
  "How long the summary that is taken from the text of a post may be, in characters."
  280)

(defn- shortened
  "The `text`, cut after a whole word and marked when it is longer than `limit`."
  [limit text]
  (if (<= (count text) limit)
    text
    (str (str/replace (subs text 0 limit) #"\s+\S*$" "") "\u2026")))

(defn- first-paragraph
  "The start of the first text block of `blocks`, or nil when there is none."
  [blocks]
  (some->> blocks (filter (comp #{:text} :kind)) first plain (shortened summary-length)))

(defn from
  "The post in the folder `folder`, from what the reader read of its text. Its title is the one
   in its header block, or else the heading that opens it, or else the name of its folder. Its
   summary is the one in its header block, or else the start of its first paragraph. It keeps
   what is wrong with it under `:problems`, when something is."
  [folder {:keys [fields problems heading blocks]}]
  (let [summary (or (:summary fields) (first-paragraph blocks))]
    (cond-> (assoc fields
                   :title  (or (:title fields) heading folder)
                   :blocks blocks)
      summary        (assoc :summary summary)
      (seq problems) (assoc :problems problems))))

(def file-pattern
  "What the name of a file beside a post looks like, as the text of a pattern."
  "(?!\\.)[^/\\\\:]+")

(defn file-name?
  "True when `file-name` is the name of a file beside a post that a page can show: it is no
   hidden file, it leads nowhere else, and a page does not keep the name for itself."
  [file-name]
  (boolean (and (not (str/includes? file-name ".."))
                (not (contains? wire/reserved file-name))
                (re-matches (re-pattern file-pattern) file-name))))

(defn file-beside
  "The name of the file beside a post that the address `source` names, or nil when it names
   none. What an address asks of the file after a question mark, and the place in it that it
   names after a hash, are no part of the name."
  [source]
  (when-some [file-name (some-> source (str/replace #"[?#].*" "") address/decoded)]
    (when (file-name? file-name)
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

(def ^:private names-a-file
  "The keys under which a thing in a post holds an address."
  [:src :href])

(defn- placed-at
  "The address under `k` in `x` as the address of the file it names under `at`, when it names
   one beside the post."
  [at x k]
  (cond-> x
    (file-beside (get x k)) (update k (partial str at))))

(defn- with-place [at x]
  (if (map? x)
    (reduce (partial placed-at at) x names-a-file)
    x))

(defn placed
  "The `post` with every address that names a file beside it as the address of that file
   under `at`, the address of the post. The post then shows its files from any page, and not
   only from its own."
  [post at]
  (update post :blocks (partial walk/postwalk (partial with-place at))))
