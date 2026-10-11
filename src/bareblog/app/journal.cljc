(ns bareblog.app.journal
  "The Journal blog template: one reading column under a navbar.

   A blog template is a tree of nodes. A node has a template: `:split`, with a hole for each
   thing the view gives it, or `:fixed-of`, a function from what the view gives it to a fixed
   template. A node may have `:lists`. A list names the part that holds its rows, and says what
   node a row is: `:row` for every row, or `:rows` by the key of the row.

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
   :not-found "Not found"
   :not-here  "This page is not here."
   :to-home   "To the home page"})

(def ^:private published? (complement :draft?))
(def ^:private no-heading? (comp nil? :heading))
(def ^:private no-newer? (comp nil? :newer))
(def ^:private no-older? (comp nil? :older))
(def ^:private posts? (comp pos? :count))

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
  {:split (template/split :journal-tag [:li [:a {:href :href} :name]])})

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
            [:h1 {:hidden no-heading?} :heading]
            [:p {:class "empty" :hidden posts?} (:no-posts words)]
            [:ol {:data-x-part "entries"}]
            [:x-pagination {:label (:pages words) :page :number :total-pages :pages :hidden one-page?
                            :on    {"page-change-request" :to-page}}]])
   :lists {:entries {:part "entries" :row entry}}})

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

(defn- page-title [{:keys [number]}]
  (when (< 1 number)
    (str (:page words) " " number)))

(def ^:private titles
  "What names each kind of page in the title of the document, from what the view says the page
   is. A page with no name has the name of the blog alone."
  {:home    page-title
   :tag     :tag
   :archive (constantly (:archive words))
   :post    :post})

(defn- title
  "The name of the page in the title of the document, or nil."
  [named]
  ((get titles (:kind named) (constantly (:not-found words))) named))

(def template
  "The node of the whole document. Its one list holds the node of the page, by its key."
  {:key   "journal"
   :title title
   :split (template/split
           :journal
           [:x-theme {:preset :preset}
            [:div {:class "page"}
             [:x-navbar {:label :blog-title :sticky true}
              [:a {:slot "brand" :class "brand" :href :home} :blog-title]
              [:a {:class "nav-link" :href :home} (:home words)]
              [:a {:class "nav-link" :href :archive} (:archive words)]]
             [:x-container {:as "main"}
              [:div {:data-x-part "main"}]]]])
   :lists {:main {:part "main"
                  :rows {"post"      post
                         "entries"   entries
                         "archive"   archive
                         "not-found" not-found}}}})
