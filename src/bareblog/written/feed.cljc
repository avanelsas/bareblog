(ns bareblog.written.feed
  "The feed of a blog: its posts, each with its summary, as Atom.

   The feed is made as a tree: an element is a vector of its tag, its attributes and what it
   holds, which is text or elements. `atom-feed` writes the tree out."
  (:require [bareblog.blog.address :as address]
            [bareblog.blog.site :as site]
            [clojure.string :as str]))

(defn- entry
  "A post in the feed. `at` gives the full address of a page."
  [at {:keys [folder title date summary tags]}]
  (let [address (at {:kind :post :folder folder})]
    (into [:entry {}
           [:title {} title]
           [:link {:href address}]
           [:id {} address]
           [:updated {} date]
           (when summary [:summary {} summary])]
          (for [tag-name tags] [:category {:term tag-name}]))))

(def ^:private no-posts
  "The moment that a feed with no posts says it was changed."
  "1970-01-01T00:00:00Z")

;; A feed says when each entry was changed, which is the date of a post. A draft in a preview
;; has none, and is left out.
(defn tree
  "The feed of the blog in `site`, whose top is at `root`, as a tree: the posts that have a
   date, the newest first, each with its title, its address, its date, its summary and its
   tags. The feed is in the language of the blog."
  [root {:keys [blog posts]}]
  (let [origin (:origin (address/of-blog (:address blog)))
        at     (fn [page] (str origin (address/of-page root page)))
        dated  (filter :date posts)
        home   (at {:kind :home :number 1})]
    (into [:feed {:xmlns "http://www.w3.org/2005/Atom" :xml:lang (site/language blog)}
           [:title {} (:title blog)]
           [:id {} home]
           [:link {:href home}]
           [:link {:rel "self" :href (str origin (address/feed root))}]
           [:updated {} (or (:date (first dated)) no-posts)]
           [:author {} [:name {} (:author blog (:title blog))]]]
          (map (partial entry at) dated))))

(def ^:private escapes
  {\& "&amp;" \< "&lt;" \> "&gt;" \" "&quot;"})

(defn- text
  "The text `x` as it is written inside an element or an attribute."
  [x]
  (str/escape (str x) escapes))

(defn- attributes [attrs]
  (str/join (for [[k v] attrs :when (some? v)] (str " " (name k) "=\"" (text v) "\""))))

(defn- written
  "The element `element` as text, each element that holds elements on lines of its own."
  [indent [tag attrs & held]]
  (let [held  (remove nil? held)
        open  (str indent "<" (name tag) (attributes attrs))
        close (str "</" (name tag) ">")]
    (cond
      (empty? held)        (str open "/>")
      (every? vector? held) (str open ">\n"
                                 (str/join "\n" (map (partial written (str indent "  ")) held))
                                 "\n" indent close)
      :else                (str open ">" (text (str/join held)) close))))

(defn atom-feed
  "The feed of the blog in `site`, whose top is at `root`, as the text of an Atom document."
  [root site]
  (str "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n" (written "" (tree root site)) "\n"))
