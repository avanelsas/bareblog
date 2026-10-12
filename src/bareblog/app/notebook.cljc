(ns bareblog.app.notebook
  "The Notebook blog template: a sidebar that lists every post, beside what the page shows.

   See `bareblog.app.templates` for what a blog template is."
  (:require [bareblog.app.pages :as pages]
            [baremirror.alpha.template :as template]))

(defn- current
  "Whether a row of the sidebar is the post of this page, as the text that an ARIA state takes."
  [{:keys [current?]}]
  (when current? "page"))

(def ^:private sidebar-row
  "A post in the sidebar. The first of its year shows the year above it."
  {:split (template/split
           :notebook-row
           [:li {:class "sidebar-row"}
            [:span {:class "year"} pages/year-shown]
            [:a {:href :href :aria-current current} :title]
            pages/draft])})

(defn- search-key
  "The key that opens the palette, as the keyboard of the reader names it."
  [{:keys [command-key?]}]
  ((if command-key? :command :control) (:search-key pages/words)))

(def ^:private search-button
  "The button that opens the palette, with the key that does the same."
  [:button {:type "button" :class "search-button" :on {"click" :open-palette}}
   [:span (:search pages/words)]
   [:kbd search-key]])

(def ^:private no-tag? (comp nil? :tag))

(def ^:private clear-button
  "The way back to every post, for a reader who chose a tag."
  [:button {:type "button" :class "clear" :hidden no-tag? :on {"click" :clear}}
   (:clear pages/words)])

(def ^:private sidebar-shown?
  "True when the sidebar is there: beside the page in a wide window, or as the menu when the
   reader opened that."
  (some-fn :wide? :menu?))

;; The palette takes its commands as a property, which BareMirror does not write. `:props`
;; names the part, the property and what gives its value, and the renderer writes it.
(def template
  "The node of the whole document. Its lists hold the node of the page, by its key, the posts
   and the tags of the sidebar, and the presets of the theme select."
  {:key        "notebook"
   :title      pages/title
   :home       :newest-post
   :choice     :everywhere
   :search     :palette
   :split      (template/split
                :notebook
                [:x-theme {:preset :preset}
                 [:div {:class "page notebook"}
                  (pages/navbar search-button)
                  [:x-command-palette {:label       (:search pages/words)
                                       :placeholder (:search pages/words)
                                       :empty-text  (:none (:found pages/words))
                                       :open        :palette?
                                       :data-x-part "palette"
                                       :on          {"x-command-palette-open-request"  :open-palette
                                                     "x-command-palette-close-request" :close-palette
                                                     "x-command-palette-select"        :open-post}}]
                  [:div {:class "columns"}
                   [:x-sidebar {:label      (:posts pages/words)
                                :breakpoint pages/wide-from
                                :open       sidebar-shown?
                                :on         {"toggle" :sidebar}}
                    [:div {:class "sidebar"}
                     (-> [:div {:class "menu"}]
                         (into pages/links)
                         (conj search-button (pages/theme-select "menu-presets")))
                     [:ul {:class "tag-choices" :aria-label (:all-tags pages/words) :data-x-part "tags"}]
                     clear-button
                     [:ol {:class "sidebar-posts" :data-x-part "posts"}]]]
                   [:x-container {:as "main"}
                    [:div {:data-x-part "main"}]]]]])
   :props      {"palette" {:items :commands}}
   :lists      {:presets      {:part "presets" :row pages/preset}
                :menu-presets {:part "menu-presets" :row pages/preset :of :presets}
                :tags         {:part "tags" :row pages/tag-choice}
                :posts        {:part "posts" :row sidebar-row}
                :main         {:part "main" :rows pages/page-rows}}})
