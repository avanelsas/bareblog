(ns bareblog.app.longform
  "The Longform blog template: Journal, with the posts of a list as a deck of large cards that
   pile up as the reader scrolls, and a post that opens on its first picture.

   See `bareblog.app.templates` for what a blog template is."
  (:require [bareblog.app.journal :as journal]
            [bareblog.app.pages :as pages]
            [baremirror.alpha.template :as template]))

(def ^:private card
  "An entry as a large card: its picture, its title, its date and tags, and its summary."
  {:split (template/split
           :longform-card
           [:article {:class "deck-card" :role "listitem"}
            pages/cover
            pages/card-text])
   :lists {:tags {:part "tags" :row pages/tag}}})

(def ^:private deck
  "The node of a page that lists posts, as a deck of cards."
  (pages/listing :longform-entries
                 [:x-scroll-stack {:class           "deck"
                                   :role            "list"
                                   :align           "top"
                                   :peek            12
                                   :rotation        2
                                   :scroll-distance 320
                                   :data-x-part     "entries"}]
                 card))

(defn- hero-class
  "The class of the opening of a post: one with a picture behind its title, or one without."
  [{:keys [picture]}]
  (if picture "hero with-picture" "hero"))

;; The picture moves at half the speed of the page, and the title at the speed of the page.
(def ^:private hero
  "The opening of a post: its first picture, behind its title, date and tags."
  [:x-scroll-parallax {:class hero-class}
   [:img {:class "hero-picture" :src :picture :alt "" :data-speed "0.5" :hidden pages/no-picture?}]
   [:div {:class "hero-text" :data-speed "1"} pages/post-header]])

(def template
  "The node of the whole document."
  (journal/shell :longform (assoc pages/page-rows
                                  "entries" deck
                                  "post"    (pages/post-page :longform-post hero))))
