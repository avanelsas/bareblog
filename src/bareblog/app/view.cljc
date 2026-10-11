(ns bareblog.app.view
  "What a page shows, as a value."
  (:require [bareblog.app.templates :as templates]))

(def between " · ")

(def default-language "en")

(defn- with-key
  "Adds `block` under a key made from its content. A block with the same content as an earlier
   one gets a count after its key."
  [{:keys [order by-key seen]} block]
  (let [content (str (hash block))
        n       (inc (get seen content 0))
        k       (if (= 1 n) content (str content "-" n))]
    {:order  (conj order k)
     :by-key (assoc by-key k block)
     :seen   (assoc seen content n)}))

(defn- keyed
  "The keys of `blocks` in order, and each block by key."
  [blocks]
  (select-keys (reduce with-key {:order [] :by-key {} :seen {}} blocks) [:order :by-key]))

(defn post-page
  "What the page of `post` shows in the blog `blog`: the name of the blog template, the
   language and the title of the document, the item of the page, the tags, the keys of the
   blocks in order, and each block by key."
  [blog post]
  (merge {:template (:template blog templates/default)
          :language (:language blog default-language)
          :title    (str (:title post) between (:title blog))
          :item     {:preset     (first (:presets blog))
                     :blog-title (:title blog)
                     :title      (:title post)
                     :date       (:date post)}
          :tags     (vec (distinct (:tags post)))}
         (keyed (:blocks post))))
