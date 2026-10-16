(ns bareblog.app.render
  "Brings the document to a page view."
  (:require [bareblog.app.card :as card]
            [bareblog.app.templates :as templates]
            [baremirror.alpha.parts :as parts]
            [baremirror.alpha.places :as places]
            [clojure.string :as str]))

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

(def ^:private last-set
  "The symbol under which an element holds the value last set for each of its properties."
  (js/Symbol "bareblog.properties"))

(defn- defined? [^js element]
  (.matches element ":defined"))

;; A node is brought to its view once before it is placed, and a custom element is not yet
;; what it will be until it is in the document. A property that is written before that hides
;; the one the element gets, so the write waits for the next time the node is brought.
(defn- set-property!
  "Makes `value` the property `property` of `element`, where it differs from what was set
   last. BareMirror writes text and attributes. This is the one place that writes a property."
  [^js element property value]
  (let [before (or (unchecked-get element last-set) {})]
    (when (and (defined? element) (not= value (get before property ::none)))
      (unchecked-set element (name property) (clj->js value))
      (unchecked-set element last-set (assoc before property value)))))

(defn- bring-properties!
  "Sets each property that `props` names, by part, to what its hole gives for `item`."
  [element-parts props item]
  (doseq [[part-name properties] props
          [property hole]        properties]
    (set-property! (element-parts part-name) property (hole item))))

(defn- bring!
  "Brings `element`, which is a node of `node`, to `shown`: the item of its template, the
   properties that the node names, and the rows of each of its lists."
  [^js element {:keys [split lists props] :as _node} shown]
  (when split
    (parts/write-item! element split (:item shown)))
  (when (or (seq lists) (seq props))
    (let [element-parts (parts/read-parts element)]
      (bring-properties! element-parts props (:item shown))
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

;; Open Graph names its tags by a property, and the card of Twitter by a name.
(defn- name-attribute [tag-name]
  (if (str/starts-with? tag-name "og:") "property" "name"))

(defn- make-card-tag!
  "A new tag of the card in the head of `document`, named `tag-name`."
  [^js document tag-name]
  (let [tag (.createElement document "meta")]
    (.setAttribute tag card-mark tag-name)
    (.setAttribute tag (name-attribute tag-name) tag-name)
    (.append (.-head document) tag)
    tag))

(defn- named-tag [^js tag]
  [(.getAttribute tag card-mark) tag])

(defn- card-tags
  "The tags of the card in the head of `document`, by their name."
  [^js document]
  (into {} (map named-tag) (array-seq (.querySelectorAll (.-head document) (str "meta[" card-mark "]")))))

;; A page that the author edits is shown again in the same document, so a tag that its card
;; had and has not now goes.
(defn- set-card!
  "Makes the tags of the card in the head of `document` the tags `tags`, where they differ."
  [^js document tags]
  (let [shown  (card-tags document)
        wanted (into #{} (map first) tags)]
    (doseq [[tag-name ^js tag] shown
            :when (not (contains? wanted tag-name))]
      (.remove tag))
    (doseq [[tag-name content] tags
            :let [^js tag (or (shown tag-name) (make-card-tag! document tag-name))]]
      (when (not= content (.getAttribute tag "content"))
        (.setAttribute tag "content" content)))))

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
  [^js document {:keys [language title card] :as view}]
  (let [{:keys [key] :as template} (templates/named (:template view))
        nodes                      (places/sync! {:document {:parent (.-body document)}}
                                                 {:document [key]}
                                                 (fn [_k] (make! template view)))]
    (set-language! document language)
    (set-title! document title)
    (set-card! document (some-> card card/tags))
    (bring! (nodes key) template view)
    nil))
