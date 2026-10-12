(ns bareblog.output
  "What a generated blog holds, as a value: its pages, and the files that are text."
  (:require [bareblog.address :as address]
            [bareblog.app.templates :as templates]
            [bareblog.feed :as feed]
            [bareblog.site :as site]
            [bareblog.wire :as wire]
            [clojure.string :as str]))

;; A tag has a page in a directory with its name. A name with a slash would be two
;; directories, and a name of dots leads out of the directory of the tags.
(defn pageless-tag?
  "True when the tag `tag-name` cannot have a page in a blog that is written out as files."
  [tag-name]
  (boolean (re-find #"[/\\]|^\.*$" tag-name)))

(defn- pageless
  "What is wrong with the tag `tag-name`, which cannot have a page."
  [tag-name]
  (str "The tag " (pr-str tag-name) " gets no page when the blog is published. A tag with a slash "
       "in it, or of dots alone, cannot name a directory."))

(defn problems
  "What is wrong with the blog in `site` as a blog that is written out as files, as texts."
  [site]
  (map pageless (filter pageless-tag? (site/tags site))))

(defn pages
  "Every page of the blog in `site`: the pages of the home page, the page of each tag that can
   have one, the archive, the page of each post, and the page for an address with no page."
  [{:keys [blog posts] :as site}]
  (concat (for [number (range 1 (inc (templates/home-pages (templates/of-blog blog) site)))]
            {:kind :home :number number})
          (for [tag-name (remove pageless-tag? (site/tags site))]
            {:kind :tag :tag tag-name})
          [{:kind :archive}]
          (for [{:keys [folder]} posts]
            {:kind :post :folder folder})
          [{:kind :not-found}]))

(defn- from-top
  "The address `address` of a blog at `root` as the name of a file from the top of the blog,
   with each percent escape as its character."
  [root address]
  (address/decoded (subs address (count root))))

(defn file-of
  "The file that holds the page `page`, from the top of a blog at `root`. The page of a
   directory is the index of that directory."
  [root page]
  (let [path (from-top root (address/of-page root page))]
    (if (or (= "" path) (str/ends-with? path "/"))
      (str path wire/index)
      path)))

(defn data-of
  "The file that holds the data of the page `page`, beside the page. The page for an address
   with no page has none, since it is sent for any address."
  [root page]
  (when-not (= :not-found (:kind page))
    (str (from-top root (address/of-page root page)) wire/page-data)))

;; What follows is for GitHub Pages, which is the host that a blog is published to. It sends
;; the files as they are when there is a `.nojekyll`, and it serves a domain of the author when
;; a `CNAME` names it.

(def ^:private as-they-are ".nojekyll")

(def ^:private domain-file "CNAME")

(def ^:private given-domain
  "The end of the host that GitHub Pages gives a blog, which needs no naming."
  ".github.io")

(defn- own-domain
  "The host of a blog when it is a domain of the author, or nil."
  [host]
  (when (and host (not (str/ends-with? host given-domain)))
    host))

(defn- host-files
  "The files that tell the host how to send the blog with the settings `blog`."
  [blog]
  (let [domain (own-domain (:host (address/of-blog (:address blog))))]
    (cond-> {as-they-are ""}
      domain (assoc domain-file (str domain "\n")))))

(defn texts
  "The files of the blog in `site`, whose top is at `root`, that are text and belong to no
   page, by their name from the top: the data of the site as its readers load it, the feed,
   and what tells the host how to send the blog."
  [root {:keys [blog] :as site}]
  (merge {wire/site-data (pr-str (site/published site))
          wire/feed      (feed/atom-feed root site)}
         (host-files blog)))
