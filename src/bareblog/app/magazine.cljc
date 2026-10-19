(ns bareblog.app.magazine
  "The Magazine blog template: Journal, with the posts of a list as cards in a grid.

   See `bareblog.app.templates` for what a blog template is."
  (:require [bareblog.app.journal :as journal]
            [bareblog.app.pages :as pages]
            [baremirror.alpha.template :as template]))

(defn- lead?
  "True when the card of an entry leads the others: the newest post with a picture on the first
   page of the home page. A large card of text would be mostly empty."
  [{:keys [front? first-picture?]}]
  (boolean (and front? first-picture?)))

(defn- lead-class [item]
  (when (lead? item) "lead"))

(defn- span
  "How many columns and rows the card of an entry spans: two for the card that leads."
  [item]
  (if (lead? item) 2 1))

;; The card that leads is shown first, and stays where it is among the others in the list.
(defn- place
  "Where the card of an entry is shown in the grid: first for the card that leads, and else in
   the order of the list."
  [item]
  (when (lead? item) -1))

;; The link of the title covers the whole card, so the card is one link. `x-card` with
;; `interactive` would be a button.
(def ^:private card
  "An entry as a card: its picture, its title, its date and tags, and its summary."
  {:split (template/split
           :magazine-card
           [:x-bento-item {:class    lead-class
                           :role     "listitem"
                           :col-span span
                           :row-span span
                           :order    place}
            [:x-card {:class "card" :variant "outlined" :padding "none"}
             [:img {:class "cover" :src :picture :alt "" :loading "lazy" :hidden pages/no-picture?}]
             [:div {:class "card-text"}
              [:h2 [:a {:href :href} :title]]
              [:div {:class "meta"}
               [:time {:class "date"} :date]
               pages/draft
               [:ul {:class "tags" :data-x-part "tags"}]]
              [:p {:class "summary"} :summary]]]])
   :lists {:tags {:part "tags" :row pages/tag}}})

(def ^:private cards
  "The node of a page that lists posts, as cards in a grid."
  (pages/listing :magazine-entries
                 [:x-bento-grid {:class       "cards"
                                 :columns     4
                                 :gap         "lg"
                                 :role        "list"
                                 :data-x-part "entries"}]
                 card))

(def template
  "The node of the whole document."
  (journal/shell :magazine (assoc pages/page-rows "entries" cards)))
