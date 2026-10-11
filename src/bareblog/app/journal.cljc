(ns bareblog.app.journal
  "The Journal blog template: one reading column under a navbar."
  (:require [bareblog.app.blocks :as blocks]
            [baremirror.alpha.template :as template]))

(def ^:private page
  "A page, with a hole for each thing that the view gives it."
  [:x-theme {:preset :preset}
   [:div {:class "page"}
    [:x-navbar {:label :blog-title :sticky true}
     [:a {:slot "brand" :class "brand" :href "/"} :blog-title]]
    [:x-container {:as "main"}
     [:article {:class "post"}
      [:header {:class "post-header"}
       [:h1 :title]
       [:div {:class "post-meta"}
        [:time {:class "post-date"} :date]
        [:ul {:class "post-tags" :data-x-part "tags"}]]]
      [:div {:class "post-blocks" :data-x-part "blocks"}]]]]])

(def ^:private tag
  "One tag of a post. Its item is the text of the tag."
  [:li identity])

(def template
  "What a blog template is: the split templates of a page and of a tag, the names of the parts
   that hold the tags and the blocks, and the fixed template of a block."
  {:key    "journal"
   :page   (template/split :journal page)
   :tag    (template/split :journal-tag tag)
   :tags   "tags"
   :blocks "blocks"
   :block  blocks/template})
