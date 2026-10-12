(ns bareblog.app.render
  "Brings the document to a page view."
  (:require [bareblog.app.templates :as templates]
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
  [^js document {:keys [language title] :as view}]
  (let [{:keys [key] :as template} (templates/named (:template view))
        nodes                      (places/sync! {:document {:parent (.-body document)}}
                                                 {:document [key]}
                                                 (fn [_k] (make! template view)))]
    (set-language! document language)
    (set-title! document title)
    (bring! (nodes key) template view)
    nil))
