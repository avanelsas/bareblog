(ns bareblog.app.journal
  "The Journal blog template: one reading column under a navbar.

   A blog template is a tree of nodes. A node has a template: `:split`, with a hole for each
   thing the view gives it, or `:fixed-of`, a function from what the view gives it to a fixed
   template. A node may have `:lists`. A list names the part that holds its rows, and says what
   node a row is: `:row` for every row, or `:rows` by the key of the row. A list with `:of`
   shows the rows that the view gives under that name.

   The view gives facts. What is hidden, and which words stand where, is decided here."
  (:require [bareblog.app.blocks :as blocks]
            [baremirror.alpha.template :as template]))

(def words
  "Every word that Journal puts in a page that is not from the blog."
  {:home      "Home"
   :archive   "Archive"
   :draft     "Draft"
   :pages     "Pages"
   :page      "Page"
   :others    "Other posts"
   :no-date   "No date"
   :no-posts  "There are no posts yet."
   :search    "Search"
   :theme     "Theme"
   :all-tags  "Tags"
   :clear     "Show all posts"
   :found     {:none "No posts found" :one "post found" :many "posts found"}
   :menu      "Menu"
   :close     "Close"
   :not-found "Not found"
   :not-here  "This page is not here."
   :to-home   "To the home page"})

(def ^:private published? (complement :draft?))
(def ^:private no-newer? (comp nil? :newer))
(def ^:private no-older? (comp nil? :older))
(def ^:private not-filtered? (complement :filtered?))

(defn- posts?
  "True when a list has posts, or has none because of what the reader chose."
  [{:keys [filtered?] n :count}]
  (or filtered? (pos? n)))

(defn- one-preset? [{:keys [presets]}]
  (<= presets 1))

(defn- pressed
  "Whether a tag is chosen, as the text that an ARIA state takes."
  [{:keys [chosen?]}]
  (str (boolean chosen?)))

(defn- found
  "How many posts the reader's choice found, in words."
  [n]
  (let [{:keys [none one many]} (:found words)]
    (cond
      (zero? n) none
      (= 1 n)   (str "1 " one)
      :else     (str n " " many))))

(defn- heading
  "The heading of a list: the one the view gives, or how many posts the reader's choice found."
  [{:keys [filtered?] given :heading n :count}]
  (if filtered? (found n) given))

(defn- no-heading? [item]
  (nil? (heading item)))

(defn- one-page? [{:keys [pages]}]
  (<= pages 1))

(defn- year-shown
  "The year of a row of the archive, when the row is the first of its year."
  [{:keys [year first-of-year?]}]
  (when first-of-year?
    (or year (:no-date words))))

(def ^:private draft
  "The badge of a post that is not published."
  [:x-badge {:class "draft" :text (:draft words) :variant "warning" :hidden published?}])

(def ^:private tag
  "A tag of a post. On a page that can show a choice, a click picks the tag in place."
  {:split (template/split :journal-tag [:li [:a {:href :href :on {"click" :tag}} :name]])})

(def ^:private tag-choice
  "A tag of the blog, to pick or to let go."
  {:split (template/split
           :journal-tag-choice
           [:li [:button {:type "button" :aria-pressed pressed :on {"click" :tag}} :name]])})

(def ^:private preset
  {:split (template/split :journal-preset [:option {:value :name} :name])})

(def ^:private block
  {:fixed-of blocks/template})

(def ^:private post
  {:split (template/split
           :journal-post
           [:article {:class "post"}
            [:header {:class "post-header"}
             [:h1 :title]
             [:div {:class "meta"}
              [:time {:class "date"} :date]
              draft
              [:ul {:class "tags" :data-x-part "tags"}]]]
            [:div {:class "post-blocks" :data-x-part "blocks"}]
            [:nav {:class "neighbours" :aria-label (:others words)}
             [:a {:class "newer" :rel "next" :href (comp :href :newer) :hidden no-newer?}
              (comp :title :newer)]
             [:a {:class "older" :rel "prev" :href (comp :href :older) :hidden no-older?}
              (comp :title :older)]]])
   :lists {:tags   {:part "tags" :row tag}
           :blocks {:part "blocks" :row block}}})

(def ^:private entry
  {:split (template/split
           :journal-entry
           [:li {:class "entry"}
            [:h2 [:a {:href :href} :title]]
            [:div {:class "meta"}
             [:time {:class "date"} :date]
             draft
             [:ul {:class "tags" :data-x-part "tags"}]]
            [:p {:class "summary"} :summary]])
   :lists {:tags {:part "tags" :row tag}}})

(def ^:private entries
  {:split (template/split
           :journal-entries
           [:section {:class "entries"}
            [:ul {:class "tag-choices" :aria-label (:all-tags words) :data-x-part "all-tags"}]
            [:div {:class "heading" :role "status" :hidden no-heading?}
             [:h1 heading]
             [:button {:type "button" :class "clear" :hidden not-filtered? :on {"click" :clear}}
              (:clear words)]]
            [:p {:class "empty" :hidden posts?} (:no-posts words)]
            [:ol {:data-x-part "entries"}]
            [:x-pagination {:label (:pages words) :page :number :total-pages :pages :hidden one-page?
                            :on    {"page-change-request" :to-page}}]])
   :lists {:entries  {:part "entries" :row entry}
           :all-tags {:part "all-tags" :row tag-choice}}})

(def ^:private archive-row
  {:split (template/split
           :journal-archive-row
           [:li {:class "archive-row"}
            [:span {:class "year"} year-shown]
            [:a {:href :href} :title]
            [:time {:class "date"} :date]
            draft])})

(def ^:private archive
  {:split (template/split
           :journal-archive
           [:section {:class "archive"}
            [:h1 (:archive words)]
            [:p {:class "empty" :hidden posts?} (:no-posts words)]
            [:ol {:data-x-part "rows"}]])
   :lists {:rows {:part "rows" :row archive-row}}})

(def ^:private not-found
  {:split (template/split
           :journal-not-found
           [:section {:class "not-found"}
            [:h1 (:not-here words)]
            [:p [:a {:href :home} (:to-home words)]]])})

(defn- home-title
  "What names a page of the home page: how many posts the reader's choice found, or the number
   of a later page."
  [{:keys [number] n :found}]
  (cond
    n            (found n)
    (< 1 number) (str (:page words) " " number)))

(def ^:private titles
  "What names each kind of page in the title of the document, from what the view says the page
   is. A page with no name has the name of the blog alone."
  {:home    home-title
   :tag     :tag
   :archive (constantly (:archive words))
   :post    :post})

(defn- title
  "The name of the page in the title of the document, or nil."
  [named]
  ((get titles (:kind named) (constantly (:not-found words))) named))

(def ^:private links
  "The links to the pages of the blog that every page has."
  [[:a {:class "nav-link" :href :home} (:home words)]
   [:a {:class "nav-link" :href :archive} (:archive words)]])

(def ^:private controls
  "What a reader chooses with on every page: the search and the theme."
  [[:x-search-field {:label       (:search words)
                     :placeholder (:search words)
                     :value       :query
                     :on          {"x-search-field-change-request" :search
                                   "x-search-field-search"         :submit-search}}]
   [:x-select {:label  (:theme words)
               :value  :preset
               :hidden one-preset?
               :on     {"x-select-change-request" :preset}}]])

(defn- with-part
  "The template of the theme select with the name of the part that holds its presets."
  [part-name [search select]]
  [search (update select 1 assoc :data-x-part part-name)])

;; Below its breakpoint `x-navbar` shows its brand and its toggle and nothing else. The links
;; and the controls are then in a drawer, which the toggle opens. Both places show the same
;; state, so neither has to be kept in step with the other.
(def template
  "The node of the whole document. Its lists hold the node of the page, by its key, and the
   presets of the theme select, once in the navbar and once in the menu."
  {:key   "journal"
   :title title
   :split (template/split
           :journal
           [:x-theme {:preset :preset}
            [:div {:class "page"}
             (into [:x-navbar {:label :blog-title :sticky true}
                    [:a {:slot "brand" :class "brand" :href :home} :blog-title]]
                   (conj links
                         (into [:div {:slot "actions" :class "actions"}]
                               (with-part "presets" controls))
                         [:button {:slot       "toggle"
                                   :type       "button"
                                   :class      "menu-button"
                                   :aria-label (:menu words)
                                   :on         {"click" :open-menu}}
                          "\u2630"]))
             [:x-drawer {:label (:menu words) :placement "right" :open :menu?
                         :on    {"x-drawer-toggle" :menu}}
              [:div {:slot "header" :class "menu-header"}
               [:span (:menu words)]
               [:button {:type "button" :class "menu-close" :aria-label (:close words)
                         :on   {"click" :close-menu}}
                "\u2715"]]
              (-> [:div {:class "menu"}]
                  (into links)
                  (into (with-part "menu-presets" controls)))]
             [:x-container {:as "main"}
              [:div {:data-x-part "main"}]]]])
   :lists {:presets      {:part "presets" :row preset}
           :menu-presets {:part "menu-presets" :row preset :of :presets}
           :main         {:part "main"
                          :rows {"post"      post
                                 "entries"   entries
                                 "archive"   archive
                                 "not-found" not-found}}}})
