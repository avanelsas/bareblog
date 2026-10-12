(ns bareblog.app.pages
  "The nodes that every blog template shows a page with, and the words in them.

   The view gives facts. What is hidden, and which words stand where, is decided here."
  (:require [bareblog.app.blocks :as blocks]
            [baremirror.alpha.template :as template]))

(def words
  "Every word that a blog template puts in a page that is not from the blog."
  {:home        "Home"
   :archive     "Archive"
   :draft       "Draft"
   :unpublished "Not published"
   :page-numbers "Pages"
   :page        "Page"
   :others      "Other posts"
   :no-date     "No date"
   :no-posts    "There are no posts yet."
   :search      "Search"
   :search-key  {:command "\u2318K" :control "Ctrl K"}
   :pages       "Pages"
   :theme       "Theme"
   :all-tags    "Tags"
   :clear       "Show all posts"
   :found       {:none "No posts found" :one "post found" :many "posts found"}
   :posts       "Posts"
   :menu        "Menu"
   :close       "Close"
   :not-found   "Not found"
   :not-here    "This page is not here."
   :to-home     "To the home page"})

(def wide-from
  "The width of a window in pixels from which `x-navbar` shows what it holds. A narrower window
   gets a menu button. The stylesheets write the same width, as 640 and 641 pixels."
  641)

(def published? (complement :draft?))
(def ^:private published-page? (complement :unpublished?))

(defn current
  "Whether a link leads to the page that is shown, as the text that an ARIA state takes."
  [{:keys [current?]}]
  (when current? "page"))
(def ^:private no-newer? (comp nil? :newer))
(def ^:private no-older? (comp nil? :older))
(def ^:private not-filtered? (complement :filtered?))

(defn- posts?
  "True when a list has posts, or has none because of what the reader chose."
  [{:keys [filtered?] n :count}]
  (or filtered? (pos? n)))

(defn one-preset? [{:keys [presets]}]
  (<= presets 1))

(defn pressed
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

(defn year-shown
  "The year of a row of the archive, when the row is the first of its year."
  [{:keys [year first-of-year?]}]
  (when first-of-year?
    (or year (:no-date words))))

(def draft
  "The badge of a post that is not published."
  [:x-badge {:class "draft" :text (:draft words) :variant "warning" :hidden published?}])

(def tag
  "A tag of a post, as a link to the page of the tag. The state says whether a click picks
   the tag in place."
  {:split (template/split :tag [:li [:a {:href :href :on {"click" :tag-link}} :name]])})

(def tag-choice
  "A tag of the blog, to pick or to let go."
  {:split (template/split
           :tag-choice
           [:li [:button {:type "button" :aria-pressed pressed :on {"click" :tag}} :name]])})

(def preset
  {:split (template/split :preset [:option {:value :name} :name])})

(def ^:private block
  {:fixed-of blocks/template})

(def post
  {:split (template/split
           :post
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
           :entry
           [:li {:class "entry"}
            [:h2 [:a {:href :href} :title]]
            [:div {:class "meta"}
             [:time {:class "date"} :date]
             draft
             [:ul {:class "tags" :data-x-part "tags"}]]
            [:p {:class "summary"} :summary]])
   :lists {:tags {:part "tags" :row tag}}})

(def entries
  {:split (template/split
           :entries
           [:section {:class "entries"}
            [:ul {:class "tag-choices" :aria-label (:all-tags words) :data-x-part "all-tags"}]
            [:div {:class "heading" :role "status" :hidden no-heading?}
             [:h1 heading]
             [:button {:type "button" :class "clear" :hidden not-filtered? :on {"click" :clear}}
              (:clear words)]]
            [:p {:class "empty" :hidden posts?} (:no-posts words)]
            [:ol {:data-x-part "entries"}]
            [:x-pagination {:label (:page-numbers words) :page :number :total-pages :pages :hidden one-page?
                            :on    {"page-change-request" :to-page}}]])
   :lists {:entries  {:part "entries" :row entry}
           :all-tags {:part "all-tags" :row tag-choice}}})

(def ^:private archive-row
  {:split (template/split
           :archive-row
           [:li {:class "archive-row"}
            [:span {:class "year"} year-shown]
            [:a {:href :href} :title]
            [:time {:class "date"} :date]
            draft])})

(def archive
  {:split (template/split
           :archive
           [:section {:class "archive"}
            [:h1 (:archive words)]
            [:p {:class "empty" :hidden posts?} (:no-posts words)]
            [:ol {:data-x-part "rows"}]])
   :lists {:rows {:part "rows" :row archive-row}}})

(def not-found
  {:split (template/split
           :not-found
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
   :archive    (constantly (:archive words))
   :post       :name
   :standalone :name})

(defn title
  "The name of the page in the title of the document, or nil."
  [named]
  ((get titles (:kind named) (constantly (:not-found words))) named))

(defn links
  "The links that every page has: to the home page, to the archive, and to each page of the
   author that is no post. `part-name` names the part that holds the last of these."
  [part-name]
  [[:a {:class "nav-link" :href :home} (:home words)]
   [:a {:class "nav-link" :href :archive} (:archive words)]
   [:span {:class "page-links" :data-x-part part-name}]])

(def search-field
  "The field that a reader searches with."
  [:x-search-field {:label       (:search words)
                    :placeholder (:search words)
                    :value       :query
                    :on          {"x-search-field-change-request" :search
                                  "x-search-field-search"         :submit-search}}])

(defn theme-select
  "The select that a reader picks a theme preset with. `part-name` names the part that holds
   its presets."
  [part-name]
  [:x-select {:label       (:theme words)
              :value       :preset
              :hidden      one-preset?
              :data-x-part part-name
              :on          {"x-select-change-request" :preset}}])

(def menu-button
  "The button that opens the menu, where the navbar hides what it holds."
  [:button {:slot       "toggle"
            :type       "button"
            :class      "menu-button"
            :aria-label (:menu words)
            :on         {"click" :open-menu}}
   "\u2630"])

(defn navbar
  "The navbar of a page: the name of the blog, the links, and `search` and the theme select,
   with the button that opens the menu where the navbar hides them."
  [search]
  (into [:x-navbar {:label :blog-title :sticky true}
         [:a {:slot "brand" :class "brand" :href :home} :blog-title]]
        (conj (links "pages")
              [:div {:slot "actions" :class "actions"}
               search
               (theme-select "presets")]
              menu-button)))

(def close-button
  "The button that closes the menu."
  [:button {:type "button" :class "menu-close" :aria-label (:close words) :on {"click" :close-menu}}
   "\u2715"])

(def standalone
  "A page of the author that is no post: its title and its text."
  {:split (template/split
           :standalone
           [:article {:class "post standalone"}
            [:header {:class "post-header"}
             [:h1 :title]
             [:div {:class "meta"}
              [:x-badge {:class "draft" :text (:unpublished words) :variant "warning" :hidden published-page?}]]]
            [:div {:class "post-blocks" :data-x-part "blocks"}]])
   :lists {:blocks {:part "blocks" :row block}}})

(def page-rows
  "The node of each kind of page, by its key."
  {"post"       post
   "standalone" standalone
   "entries"    entries
   "archive"    archive
   "not-found"  not-found})

(def page-link
  "The link to a page of the author that is no post."
  {:split (template/split :page-link [:a {:class "nav-link" :href :href :aria-current current} :title])})
