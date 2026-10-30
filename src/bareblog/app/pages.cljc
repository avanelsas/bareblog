(ns bareblog.app.pages
  "The nodes that every blog template shows a page with, and the words in them.

   The view gives facts. What is hidden, and which words stand where, is decided here."
  (:require [bareblog.app.blocks :as blocks]
            [bareblog.blog.wire :as wire]
            [baremirror.alpha.template :as template]))

(def words
  "Every word that a blog template puts in a page that is not from the blog."
  {:archive       "Archive"
   :feed          "Feed"
   :draft         "Draft"
   :unpublished   "Not published"
   :page-numbers  "Pages"
   :page          "Page"
   :others        "Other posts"
   :content       "Content"
   :part          "Part"
   :of            "of"
   :in-series     "In this series"
   :next-part     "Next part"
   :no-date       "No date"
   :no-posts      "There are no posts yet."
   :search        "Search"
   :search-key    {:command "\u2318K" :control "Ctrl K"}
   :pages         "Pages"
   :theme         "Theme"
   :all-tags      "Tags"
   :clear         "Show all posts"
   :found         {:none "No posts found" :one "post found" :many "posts found"}
   :posts         "Posts"
   :menu          "Menu"
   :close         "Close"
   :not-found     "Not found"
   :not-here      "This page is not here."
   :to-home       "To the home page"
   :powered-by    "Powered by "
   :comments      "Comments"
   :comment-count {:one "comment" :many "comments"}
   :markdown      "Supports Markdown"
   :copy          "Copy"
   :sending       "Sending your comment\u2026"
   :received      "Received. Your comment shows here once the author has read it."
   :not-sent      "Your comment was not sent"
   :not-sent-why  {:closed "comments are closed."
                   :name   "the name is empty or too long."
                   :text   "the comment is empty or too long."
                   :full   "the author has many comments to read. Try again later."
                   :failed "the blog could not take it just now."}
   :copy-it       "Copy it from here to send it again:"})

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
(def no-picture? (comp nil? :picture))
(def no-cover? (comp nil? :cover))
(def cover-src (comp :src :cover))
(def cover-alt (comp :alt :cover))
(def ^:private cover-ratio (comp blocks/ratio :cover))
(def ^:private not-reading? (comp nil? :read))
(def ^:private not-filtered? (complement :filtered?))
(def ^:private no-footer? (complement :powered-by-footer?))
(def ^:private logo-light (comp :light :logo))
(def ^:private logo-dark (comp :dark :logo))
(def ^:private no-logo? (comp nil? logo-light))
(def ^:private logo? (comp some? logo-light))

;; A page change moves an element into the element of the same name on the next page, so the
;; picture of a card grows into the cover of its post, and its title into the title. The class
;; is what the stylesheet calls every element of the kind.
(defn- named
  "The name of the element `kind` of the post in the folder `folder`."
  [kind folder]
  (str kind "-" folder))

(defn- transition-name
  "The style that names the element `kind` of the post of `item`, on every page that shows it,
   and gives it the class `kind`."
  [kind {:keys [folder]}]
  (str "view-transition-name: " (named kind folder) "; view-transition-class: " kind))

(def cover-name (partial transition-name "cover"))
(def ^:private title-name (partial transition-name "title"))

;; A component that moves an element writes the style of that element, and a hole there would
;; undo what it writes. Such an element takes its name from the element around it.
(defn cover-name-around
  "The style that hands the name of the cover of the post of `item` to what the element holds,
   as `--cover-name`."
  [{:keys [folder]}]
  (str "--cover-name: " (named "cover" folder)))

;; BareMirror gives a property the value as it is. The tests read it as the data it was made from.
(def as-js
  "Turns data into what a component that reads JavaScript is given."
  #?(:cljs clj->js :clj identity))

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

(def ^:private no-comment-count? (comp nil? :comment-count))

(defn- comment-count-text [{n :comment-count}]
  (str n " " (get-in words [:comment-count (if (= 1 n) :one :many)])))

(def comment-count
  "How many comments a post in a list has, where it has some."
  [:span {:class "comment-count" :hidden no-comment-count?} comment-count-text])

(def meta-line
  "What a post shows below its title: its date, how many comments it has in a list, whether it
   is a draft, and its tags."
  [:div {:class "meta"}
   [:time {:class "date"} :date]
   comment-count
   draft
   [:ul {:class "tags" :data-x-part "tags"}]])

(defn- titled
  "The title `heading`, filled with a gradient where the hole `gradient` says so, by default the
   fact `:gradient-titles?`."
  ([heading]
   (titled :gradient-titles? heading))
  ([gradient heading]
   [:x-typography {:class "title" :gradient gradient} heading]))

(defn post-header-with
  "The header of a post: its title, its date, whether it is a draft, and its tags. The hole
   `gradient` says whether its title is filled with a gradient."
  [gradient]
  [:header {:class "post-header"}
   (titled gradient [:h1 {:style title-name} :title])
   meta-line])

;; The line says no more than the scrollbar does, so it is hidden from assistive technology.
;; x-progress takes away an `aria-label` that it did not write, and shows its own label as text.
;; It is fixed to the window, which holds while no element around the post is transformed.
(def read-progress
  "How far the reader has scrolled through the page of a post, as a line along the top of the
   window. A page with nothing to scroll through hides it."
  [:x-progress {:class       "read-progress"
                :value       :read
                :max         1
                :aria-hidden "true"
                :hidden      not-reading?}])

;; x-toc gives each heading it lists an id from its text, so the post's blocks need none.
(def contents
  "The headings of the text of a post, as links that mark the one the reader is at."
  [:x-toc {:class "post-toc" :for "post-text" :label (:content words) :sticky true}])

(defn- part-label [{:keys [number]}]
  (str (:part words) " " number))

(defn- part-status
  "Where a part stands on the path of its series: read, the one being read, or still to come."
  [{:keys [current? read?]}]
  (cond current? "active" read? "complete" :else "pending"))

(defn- current-page [{:keys [current?]}]
  (when current? "page"))

(def ^:private series-part
  "A part of a series, as a step on its path, with its title as the link to it."
  {:split (template/split
           :series-part
           [:x-timeline-item {:label part-label :status part-status}
            [:a {:href :href :aria-current current-page} :title]])})

(defn- series-header
  "The name of the series of a post, and where the post stands in it."
  [{{series-name :name n :count :keys [number]} :series}]
  (str series-name " \u00b7 " (:part words) " " number " " (:of words) " " n))

(def ^:private no-series? (comp nil? :series))
(def ^:private next-part (comp :next :series))
(def ^:private no-next? (comp nil? next-part))

(defn- next-label [item]
  (str (:next-part words) ": " (:title (next-part item))))

;; The series stands at the foot of a post, beside the other posts, so that a reader at the end
;; of a part chooses between the two. It opens on every part, and the next one is beside it.
(def ^:private series-group
  "The series that the post is a part of: where the post stands in it, folded over every part
   in order, and the link to the next part, which the last part has none of."
  [:x-card {:class "series-group" :variant "elevated" :radius "lg" :padding "md" :hidden no-series?}
   [:p {:class "foot-label"} (:in-series words)]
   [:div {:class "series-line"}
    [:x-collapse {:class "series" :header series-header}
     [:x-timeline {:slot "content" :data-x-part "parts"}]]
    [:a {:class "series-next" :href (comp :href next-part) :aria-label next-label :hidden no-next?}
     (comp :title next-part)]]])

(def ^:private comment-depth
  "The deepest indent of a reply. A deeper one stands there, and says whom it answers."
  3)

(defn- short-id-line
  "The short id of a comment, with a button that copies it, for its author in a preview."
  [short-id]
  [:span {:slot "meta" :class "short-id"}
   [:code short-id]
   [:x-copy {:text short-id} (:copy words)]])

(defn- comment-template
  "The fixed template of a comment, with its blocks as its text."
  [{:keys [id name day depth reply-to-name author? short-id blocks]}]
  (into [:x-comment (blocks/known {:comment-id id :name name :datetime day :depth (str depth)
                                   :max-depth (str comment-depth) :reply-to-name reply-to-name
                                   :author (when author? true)})]
        (cond-> (mapv blocks/template blocks)
          short-id (conj (short-id-line short-id)))))

(def ^:private a-comment
  {:fixed-of comment-template})

(def ^:private no-comments? (complement :comments?))

(defn- unless-sent
  "True of an item whose last send has nothing under `k`."
  [k]
  (complement (comp k :sent)))

(defn- not-sent-line [item]
  (str (:not-sent words) ": " (get-in words [:not-sent-why (get-in item [:sent :error])]) " "
       (:copy-it words)))

;; The form clears when a comment is sent, before the script answers. A comment that was not
;; sent is shown under the form, so that the reader can copy it.
(def ^:private comment-notice
  "What stands under the form: that a comment is on its way, and what came of the last send."
  [:div {:slot "notice" :class "comment-notice"}
   [:p {:class "comment-sending" :hidden (unless-sent :sending?) :role "status"} (:sending words)]
   [:p {:class "comment-received" :hidden (unless-sent :received?)} (:received words)]
   [:div {:class "comment-not-sent" :hidden (unless-sent :error)}
    [:p not-sent-line]
    [:pre (comp :text :sent)]]])

;; The title of the form is inside x-comments. `blog.css` puts the words of the hint after it,
;; from the custom property that reaches into the component.
(def ^:private comment-thread
  "The comments of a post under its text, with the form to send one while comments are on."
  [:x-comments {:class          "comments"
                :style          (str "--comments-hint: \"" (:markdown words) "\"")
                :closed         :closed?
                :reply-to       :reply-to
                :commenter-name :commenter
                :name-max       (str (:name-max wire/comment-limits))
                :text-max       (str (:text-max wire/comment-limits))
                :heading-label  (:comments words)
                :hidden         no-comments?
                :data-x-part    "comments"
                :on             {"x-comments-reply-request" :reply-to
                                 "x-comments-submit"        :send-comment}}
   comment-notice])

(defn- post-layout
  "The text of a post beside its contents, in an article that opens with the elements
   `opening`."
  [opening]
  [:div {:class "post-layout"}
   (-> [:article {:class "post"} read-progress]
       (into opening)
       (conj [:div {:class "post-blocks" :id "post-text" :data-x-part "blocks"}]
             comment-thread
             [:footer {:class "post-foot"}
              series-group
              [:nav {:class "foot-group neighbours" :aria-label (:others words)}
               [:p {:class "foot-label"} (:others words)]
               [:a {:class "newer" :rel "next" :href (comp :href :newer) :hidden no-newer?}
                (comp :title :newer)]
               [:a {:class "older" :rel "prev" :href (comp :href :older) :hidden no-older?}
                (comp :title :older)]]]))
   contents])

(defn- post-node [node-name root]
  {:split (template/split node-name root)
   :lists {:tags     {:part "tags" :row tag}
           :blocks   {:part "blocks" :row block}
           :parts    {:part "parts" :row series-part}
           :comments {:part "comments" :row a-comment}}})

(defn post-page
  "The node of the page of a post, named `node-name`, whose article opens with `header`: an
   element that holds a header from `post-header-with`."
  [node-name header]
  (post-node node-name (post-layout [header])))

(defn post-page-under
  "The node of the page of a post, named `node-name`, with `opening` across the page above its
   text and its contents: an element that holds a header from `post-header-with`."
  [node-name opening]
  (post-node node-name [:div {:class "post-page"} opening (post-layout [])]))

(defn hero-class
  "The class of the opening of a post: one with its cover behind its title, or one without."
  [{:keys [cover]}]
  (if cover "hero with-cover" "hero"))

(defn gradient-title?
  "Whether the title of a post is filled with a gradient: never over its cover, where it is
   white."
  [{:keys [gradient-titles? cover]}]
  (boolean (and gradient-titles? (not cover))))

(def ^:private post-cover
  "The cover of a post."
  [:figure {:class "post-cover" :style cover-name :hidden no-cover?}
   [:x-image {:src cover-src :alt cover-alt :ratio cover-ratio}]])

(def post
  "The page of a post."
  (post-page :post [:div {:class "opening"} (post-header-with :gradient-titles?) post-cover]))

(def card-picture
  "The first picture of a post, across the top of a card."
  [:img {:class "cover" :src :picture :alt "" :loading "lazy" :style cover-name
         :hidden no-picture?}])

;; The link of the title covers the whole card, so the card is one link.
(defn card-text-with
  "What a card says of a post: its title, as the link to it, its date and tags, and its summary.
   The hole `gradient` says whether its title is filled with a gradient."
  [gradient]
  [:div {:class "card-text"}
   (titled gradient [:h2 {:style title-name} [:a {:href :href} :title]])
   meta-line
   [:p {:class "summary"} :summary]])

(def card-text
  (card-text-with :gradient-titles?))

(def entry
  "A post in a list: its title, its date and tags, and its summary beside its first picture."
  {:split (template/split
           :entry
           [:li {:class "entry"}
            (titled [:h2 {:style title-name} [:a {:href :href} :title]])
            meta-line
            [:div {:class "abstract"}
             [:img {:class "thumbnail" :src :picture :alt "" :loading "lazy" :style cover-name
                    :hidden no-picture?}]
             [:p {:class "summary"} :summary]]])
   :lists {:tags {:part "tags" :row tag}}})

(defn listing
  "The node of a page that lists posts, named `node-name`. `holder` holds the entries in the part
   \"entries\", and `row` is the node of each entry."
  [node-name holder row]
  {:split (template/split
           node-name
           [:section {:class "entries"}
            [:ul {:class "tag-choices" :aria-label (:all-tags words) :data-x-part "all-tags"}]
            [:div {:class "heading" :role "status" :hidden no-heading?}
             [:h1 heading]
             [:button {:type "button" :class "clear" :hidden not-filtered? :on {"click" :clear}}
              (:clear words)]]
            [:p {:class "empty" :hidden posts?} (:no-posts words)]
            holder
            [:x-pagination {:label (:page-numbers words) :page :number :total-pages :pages :hidden one-page?
                            :on    {"page-change-request" :to-page}}]])
   :lists {:entries  {:part "entries" :row row}
           :all-tags {:part "all-tags" :row tag-choice}}})

(def entries
  "The node of a page that lists posts, one below the other."
  (listing :entries [:ol {:data-x-part "entries"}] entry))

(def ^:private archive-row
  {:split (template/split
           :archive-row
           [:li {:class "archive-row"}
            [:span {:class "year"} year-shown]
            [:a {:href :href} :title]
            comment-count
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
  "The links that every page has: to the archive, to each page of the author that is no
   post, and to the feed. `part-name` names the part that holds the pages of the author. The
   name of the blog leads to the home page, so no link does."
  [part-name]
  [[:a {:class "nav-link" :href :archive} (:archive words)]
   [:span {:class "page-links" :data-x-part part-name}]
   [:a {:class "nav-link" :href :feed :type wire/feed-type} (:feed words)]])

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

(def dark-mode
  "The media query of a theme in its dark mode, which a picture for that mode is shown in."
  "(prefers-color-scheme: dark)")

(def brand
  "The brand of the navbar, which leads to the home page: the logo of the blog, with the name
   of the blog as its text, or else the name."
  [:a {:slot "brand" :class "brand" :href :home}
   [:picture {:hidden no-logo?}
    [:source {:media dark-mode :srcset logo-dark}]
    [:img {:class "logo" :src logo-light :alt :blog-title}]]
   [:span {:hidden logo?} :blog-title]])

(def ^:private theme-colors
  "The three colours of the theme, as x-gaussian-blur takes them."
  "var(--x-color-primary), var(--x-color-secondary), var(--x-color-tertiary)")

(def page-colors
  "The colours of the theme, still, behind the whole page when the item says so."
  [:x-gaussian-blur {:class       "page-colors"
                     :colors      theme-colors
                     :animation   "none"
                     :blur        "50"
                     :count       "5"
                     :size        "large"
                     :opacity     "0.25"
                     :data-colors :page-colors?}])

(def page-texture
  "The texture over the background of the whole page, which the item names: none, grain or
   paper."
  [:div {:class "page-texture" :aria-hidden "true" :data-texture :texture}])

(defn navbar
  "The navbar of a page, in a band that shows the colours of the theme when the item says so:
   the brand, the links, and `search` and the theme select, with the button that opens the
   menu where the navbar hides them."
  [search]
  [:x-gaussian-blur {:class       "navbar-band"
                     :colors      theme-colors
                     :blur        "10"
                     :count       "4"
                     :size        "60"
                     :speed       "slow"
                     :opacity     "0.7"
                     :data-colors :navbar-colors?}
   (into [:x-navbar {:label :blog-title}
          brand]
         (conj (links "pages")
               [:div {:slot "actions" :class "actions"}
                search
                (theme-select "presets")]
               menu-button))])

(defn favicon-tags
  "The tags of the head that name the favicon of the blog, for the addresses of its pictures
   by mode in `favicon`, in order: each a `:href`, with the `:media` it is for where that is
   one mode."
  [{:keys [light dark]}]
  (cond-> []
    light (conj {:href light})
    dark  (conj {:href dark :media dark-mode})))

(def bareblog-address
  "Where BareBlog is, for a reader who wants to know what made the blog."
  "https://github.com/avanelsas/bareblog")

(def footer
  "The line at the bottom of every page, which says what made the blog. The settings of a blog
   can leave it out."
  [:footer {:class "site-footer" :hidden no-footer?}
   (:powered-by words)
   [:a {:href bareblog-address} "BareBlog"]])

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
             (titled [:h1 :title])
             [:div {:class "meta"}
              [:x-badge {:class "draft" :text (:unpublished words) :variant "warning" :hidden published-page?}]]]
            [:div {:class "post-blocks" :data-x-part "blocks"}]])
   :lists {:blocks {:part "blocks" :row block}}})

(def main
  "The column that holds the node of the page."
  [:x-container {:as "main" :size "xl"}
   [:div {:data-x-part "main"}]])

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
