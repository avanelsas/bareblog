(ns bareblog.app.render
  "Brings the document to a page view."
  (:require [bareblog.app.templates :as templates]
            [baremirror.alpha.parts :as parts]
            [baremirror.alpha.places :as places]))

(defn- make-page! [{:keys [page]} _k]
  (parts/make-node! (:fixed page)))

(defn- make-tag! [{:keys [tag]} k]
  (doto (parts/make-node! (:fixed tag))
    (parts/write-item! tag k)))

(defn- make-block! [{:keys [block]} by-key k]
  (parts/make-node! (block (by-key k))))

(defn- page-node!
  "The node of the page of `template` in `root`. It is made when `root` has none."
  [^js root {:keys [key] :as template}]
  (get (places/sync! {:page {:parent root}} {:page [key]} (partial make-page! template)) key))

(defn- set-title!
  "Makes `title` the title of `document`, where it differs."
  [^js document title]
  (when (not= title (.-title document))
    (set! (.-title document) title)))

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

(defn- set-language!
  "Makes `language` the language of `document`, where it differs."
  [^js document language]
  (let [^js root (.-documentElement document)]
    (when (not= language (.-lang root))
      (set! (.-lang root) language))))

(defn render!
  "Brings `document` to the page view `view`."
  [^js document {:keys [language title item tags order by-key] :as view}]
  (let [template   (templates/named (:template view))
        node       (page-node! (.-body document) template)
        page-parts (parts/read-parts node)]
    (set-language! document language)
    (set-title! document title)
    (parts/write-item! node (:page template) item)
    (places/sync! {:tags {:parent (page-parts (:tags template))}}
                  {:tags tags}
                  (partial make-tag! template))
    (places/sync! {:blocks {:parent (page-parts (:blocks template))}}
                  {:blocks order}
                  (partial make-block! template by-key))
    nil))
