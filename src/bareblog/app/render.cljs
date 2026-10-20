(ns bareblog.app.render
  "Brings the document to a page view."
  (:require [bareblog.app.card :as card]
            [bareblog.app.pages :as pages]
            [bareblog.app.templates :as templates]
            [baremirror.alpha.parts :as parts]
            [baremirror.alpha.places :as places]))

(def ^:private maker
  "A function that makes a node of a fixed template, by copying one. One is kept for each
   fixed template."
  (memoize parts/node-maker))

(declare bring!)

(defn- make!
  "A new node of `node` for `shown`, brought to it before it is placed."
  [{:keys [split fixed-of] :as node} shown]
  (doto (if split
          ((maker (:fixed split)))
          (parts/make-node! (fixed-of shown)))
    (bring! node shown)))

(defn- bring-list!
  "Brings the part of `element-parts` that holds the rows of `row-list` to the rows in `shown`."
  [element-parts list-name row-list {:keys [order by-key]}]
  (let [node-of (partial templates/row-node row-list)
        nodes   (places/sync! {list-name {:parent (element-parts (:part row-list))}}
                              {list-name order}
                              (fn [k] (make! (node-of k) (by-key k))))]
    (doseq [k order]
      (bring! (nodes k) (node-of k) (by-key k)))))

(defn- bring!
  "Brings `element`, which is a node of `node`, to `shown`: the item of its template, and the
   rows of each of its lists."
  [^js element {:keys [split lists] :as _node} shown]
  (when split
    (parts/write-item! element split (:item shown)))
  (when (seq lists)
    (let [element-parts (parts/read-parts element)]
      (doseq [[list-name row-list] lists]
        (bring-list! element-parts list-name row-list (templates/rows-of row-list list-name shown))))))

(defn- set-title!
  "Makes `title` the title of `document`, where it differs."
  [^js document title]
  (when (not= title (.-title document))
    (set! (.-title document) title)))

(defn- set-language!
  "Makes `language` the language of `document`, where it differs."
  [^js document language]
  (let [^js root (.-documentElement document)]
    (when (not= language (.-lang root))
      (set! (.-lang root) language))))

(def ^:private card-mark
  "The attribute that marks a tag of the head as one of the card of the page, by its name."
  "data-card")

(defn- make-card-tag!
  "A new tag of the card in the head of `document`, named `tag-name` by the attribute
   `attribute`."
  [^js document attribute tag-name]
  (let [tag (.createElement document "meta")]
    (.setAttribute tag card-mark tag-name)
    (.setAttribute tag attribute tag-name)
    (.append (.-head document) tag)
    tag))

(defn- named-tag [^js tag]
  [(.getAttribute tag card-mark) tag])

(defn- card-tags
  "The tags of the card in the head of `document`, by their name."
  [^js document]
  (into {} (map named-tag) (array-seq (.querySelectorAll (.-head document) (str "meta[" card-mark "]")))))

(defn- content-of [^js tag]
  (.getAttribute tag "content"))

;; A page that the author edits is shown again in the same document, so a tag that its card
;; had and has not now goes.
(defn- set-card!
  "Makes the tags of the card in the head of `document` the tags `tags`, where they differ."
  [^js document tags]
  (let [shown                  (card-tags document)
        {:keys [gone written]} (card/changes (update-vals shown content-of) tags)]
    (doseq [tag-name gone]
      (.remove ^js (shown tag-name)))
    (doseq [{:keys [attribute content] tag-name :name} written
            :let [^js tag (or (shown tag-name) (make-card-tag! document attribute tag-name))]]
      (.setAttribute tag "content" content))))

(defn- icon-tags
  "The tags of the head of `document` that name a favicon."
  [^js document]
  (array-seq (.querySelectorAll (.-head document) "link[rel=icon]")))

(defn- icon-of
  "What the favicon tag `tag` names: its `:href`, with its `:media` where it has one."
  [^js tag]
  (cond-> {:href (.getAttribute tag "href")}
    (.hasAttribute tag "media") (assoc :media (.getAttribute tag "media"))))

(defn- remove! [^js element]
  (.remove element))

(defn- make-icon-tag!
  "A new favicon tag in the head of `document`, for `icon`."
  [^js document {:keys [href media]}]
  (let [tag (.createElement document "link")]
    (.setAttribute tag "rel" "icon")
    (.setAttribute tag "href" href)
    (when media
      (.setAttribute tag "media" media))
    (.append (.-head document) tag)))

;; A browser takes the first favicon tag whose media fits, so the tags are written again in
;; their order where any of them differs.
(defn- set-icons!
  "Makes the favicon tags in the head of `document` those of `icons`, where they differ."
  [^js document icons]
  (let [shown (icon-tags document)]
    (when (not= icons (map icon-of shown))
      (run! remove! shown)
      (run! (partial make-icon-tag! document) icons))))

(def ^:private notice-id "bareblog-notice")

(defn- make-notice!
  "A notice at the top of `document`, with no text yet."
  [^js document]
  (let [notice (.createElement document "p")]
    (.setAttribute notice "id" notice-id)
    (.setAttribute notice "role" "alert")
    (.prepend (.-body document) notice)
    notice))

(defn notice!
  "Shows `text` in a notice at the top of `document`. With no text, the notice goes."
  [^js document text]
  (let [^js shown (.getElementById document notice-id)]
    (cond
      (some? text) (parts/set-text! (or shown (make-notice! document)) text)
      (some? shown) (.remove shown))))

(defn render!
  "Brings `document` to the page view `view`."
  [^js document {:keys [language title card favicon] :as view}]
  (let [{:keys [key] :as template} (templates/named (:template view))
        nodes                      (places/sync! {:document {:parent (.-body document)}}
                                                 {:document [key]}
                                                 (fn [_k] (make! template view)))]
    (set-language! document language)
    (set-title! document title)
    (set-card! document (some-> card card/tags))
    (set-icons! document (pages/favicon-tags favicon))
    (bring! (nodes key) template view)
    nil))
