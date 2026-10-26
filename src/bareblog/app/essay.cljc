(ns bareblog.app.essay
  "The Essay blog template: Journal, with the posts of a list under their covers, and a post that
   opens on its cover across the page, with its contents and its notes in the margins.

   See `bareblog.app.templates` for what a blog template is."
  (:require [bareblog.app.journal :as journal]
            [bareblog.app.pages :as pages]
            [baremirror.alpha.template :as template]))

(defn- entry-class
  "The class of a post in a list: one with its picture behind its title, or one without."
  [{:keys [picture]}]
  (if picture "essay-entry with-cover" "essay-entry"))

(defn- gradient-title?
  "Whether the title of a post in a list is filled with a gradient: never over its picture,
   where it is white."
  [{:keys [gradient-titles? picture]}]
  (boolean (and gradient-titles? (not picture))))

(def ^:private entry
  "A post in a list: its picture with its title over it, its date and tags, and its summary."
  {:split (template/split
           :essay-entry
           [:li {:class entry-class}
            pages/card-picture
            (pages/card-text-with gradient-title?)])
   :lists {:tags {:part "tags" :row pages/tag}}})

(def ^:private entries
  "The node of a page that lists posts, one below the other."
  (pages/listing :essay-entries [:ol {:class "essays" :data-x-part "entries"}] entry))

(def ^:private hero
  "The opening of a post across the page: its cover, behind its title, date and tags."
  [:div {:class pages/hero-class}
   [:img {:class "hero-picture" :src pages/cover-src :alt pages/cover-alt :style pages/cover-name
          :hidden pages/no-cover?}]
   [:div {:class "hero-text"} (pages/post-header-with pages/gradient-title?)]])

(def template
  "The node of the whole document."
  (journal/shell :essay (assoc pages/page-rows
                               "entries" entries
                               "post"    (pages/post-page-under :essay-post hero))))
