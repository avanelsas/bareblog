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

(defn- own-picture
  "The picture `picture` of a text, at its full address, or nil when it is outside the blog."
  [root blog-address picture]
  (with-address picture (full root blog-address (:src picture))))

(defn- card-picture
  "The picture of a card: the first of the text `shown` that is in the blog, or else the link
   picture of the blog."
  [root blog-address link-picture shown]
  (or (some (partial own-picture root blog-address)
            (filter shows-picture? (tree-seq coll? seq (:blocks shown))))
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

;; Open Graph names its tags by a property, and the card of Twitter by a name.
(defn- tag
  "The tag of the head named `tag-name`, with `content`: its `:attribute`, the one that names
   it, its `:name` and its `:content`."
  [[tag-name content]]
  {:attribute (if (str/starts-with? tag-name "og:") "property" "name")
   :name      tag-name
   :content   content})

(defn tags
  "The tags of Open Graph that spell out `card`, in order."
  [{:keys [title kind site-name url description picture]}]
  (let [{:keys [at width height]} picture
        twitter-card              (if (and picture (wide? picture)) "summary_large_image" "summary")]
    (mapv tag
          (cond-> [["og:title" title] ["og:type" (name kind)] ["og:site_name" site-name]]
            url                (conj ["og:url" url])
            description        (conj ["og:description" description])
            at                 (conj ["og:image" at])
            (and width height) (conj ["og:image:width" (str width)] ["og:image:height" (str height)])
            :always            (conj ["twitter:card" twitter-card])))))

(defn- shown?
  "True when `shown`, each name with its content, has the tag `tag` as it is."
  [shown tag]
  (= (:content tag) (get shown (:name tag))))

(defn changes
  "What brings the tags `shown`, each name with its content, to the tags `wanted`: under
   `:gone` the names of those that go, and under `:written` each tag that is new or has new
   content."
  [shown wanted]
  (let [names (into #{} (map :name) wanted)]
    {:gone    (remove names (keys shown))
     :written (remove (partial shown? shown) wanted)}))
