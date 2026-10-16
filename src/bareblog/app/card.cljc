(ns bareblog.app.card
  "The card of a page: what a site that shows a link to the page, as a chat, reads of it. A
   card is facts, and `tags` spells them out as the tags of Open Graph."
  (:require [bareblog.blog.address :as address]
            [clojure.string :as str]))

(defn- published
  "The full address of `path`, a path from the top of the blog at `blog-address`."
  [blog-address path]
  (str (:origin blog-address) (:root blog-address) path))

(defn- full
  "The full address of `src`, an address from `root`, in the blog at `blog-address`. A full
   address stays as it is, and one outside the blog has none."
  [root blog-address src]
  (cond
    (re-find #"^https?://" src)  src
    (str/starts-with? src root) (published blog-address (subs src (count root)))))

(defn- shows-picture? [x]
  (and (map? x) (address/picture? (:src x))))

(defn- with-address
  "The size of the picture `picture`, with its full address `address` under `:at`. Nil without
   an address."
  [picture address]
  (when address
    (assoc (select-keys picture [:width :height]) :at address)))

(defn- card-picture
  "The picture of a card: the first of the text `shown`, or else the link picture of the blog."
  [root blog-address link-picture shown]
  (or (when-some [own (first (filter shows-picture? (tree-seq coll? seq (:blocks shown))))]
        (with-address own (full root blog-address (:src own))))
      (when link-picture
        (with-address link-picture (published blog-address (:src link-picture))))))

;; Another site reads a card without the page around it, so each address in it is full. A
;; blog whose settings name no address has none, and its card leaves them out.
(defn card
  "The card of the page that `told` describes. A post or a page of the author is an article,
   with its own title and summary. Any other page is the blog, by its name."
  [{:keys [site page] shown :text}]
  (let [{:keys [blog link-picture root]} site
        blog-address                     (when (:address blog) (address/of-blog (:address blog)))
        article                          (when (#{:post :standalone} (:kind page)) shown)
        found                            (when blog-address (card-picture root blog-address link-picture article))]
    (cond-> {:title     (if article (:title article) (:title blog))
             :kind      (if article :article :website)
             :site-name (:title blog)}
      blog-address       (assoc :url (full root blog-address (address/of-page root page)))
      (:summary article) (assoc :description (:summary article))
      found              (assoc :picture found))))

;; A site shows a wide picture beneath the text, and a square one small beside it. One of
;; unknown size is taken to be wide, as the picture of a post mostly is.
(defn- wide? [{:keys [width height]}]
  (or (not (and width height))
      (>= (/ width height) 1.5)))

(defn tags
  "The tags of Open Graph that spell out `card`, in order, each its name and its content."
  [{:keys [title kind site-name url description picture]}]
  (let [{:keys [at width height]} picture]
    (-> (cond-> [["og:title" title] ["og:type" (name kind)] ["og:site_name" site-name]]
          url                (conj ["og:url" url])
          description        (conj ["og:description" description])
          at                 (conj ["og:image" at])
          (and width height) (conj ["og:image:width" (str width)] ["og:image:height" (str height)]))
        (conj ["twitter:card" (if (and picture (wide? picture)) "summary_large_image" "summary")]))))
