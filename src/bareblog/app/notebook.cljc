(ns bareblog.app.notebook
  "The Notebook blog template: a sidebar that lists every post, beside what the page shows.

   See `bareblog.app.templates` for what a blog template is."
  (:require [bareblog.app.pages :as pages]
            [baremirror.alpha.template :as template]))

(def ^:private sidebar-row
  "A post in the sidebar. The first of its year shows the year above it."
  {:split (template/split
           :notebook-row
           [:li {:class "sidebar-row"}
            [:span {:class "year"} pages/year-shown]
            [:a {:href :href :aria-current pages/current} :title]
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

(defn- group
  "The name of the group of a command in the palette: its year, or what a post with no date
   and a page that is no post are under."
  [{:keys [year page?]}]
  (cond
    page? (:pages pages/words)
    year  year
    :else (:no-date pages/words)))

(defn- palette-item
  "The item of the palette for `command`, with the name of its group."
  [command]
  (assoc (dissoc command :year :page?) :group (group command)))

(defn- palette-items
  "The items of the palette: every command that the view gives, with the name of its group."
  [{:keys [commands]}]
  (mapv palette-item commands))

(def template
  "The node of the whole document. Its lists hold the node of the page, by its key, the posts
   and the tags of the sidebar, and the presets of the theme select."
  {:key        "notebook"
   :title      pages/title
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
                                       :props       {:items (comp pages/as-js palette-items)}
                                       :on          {"x-command-palette-open-request"  :open-palette
                                                     "x-command-palette-close-request" :close-palette
                                                     "x-command-palette-select"        :open-post}}]
                  [:div {:class "columns"}
                   [:x-sidebar {:label      (:posts pages/words)
                                :breakpoint pages/wide-from
                                :open       :menu?
                                :on         {"toggle" :menu}}
                    [:div {:class "sidebar"}
                     (-> [:div {:class "menu"}]
                         (into (pages/links "menu-pages"))
                         (conj search-button (pages/theme-select "menu-presets")))
                     [:ul {:class "tag-choices" :aria-label (:all-tags pages/words) :data-x-part "tags"}]
                     clear-button
                     [:ol {:class "sidebar-posts" :data-x-part "posts"}]]]
                   [:div {:class "content"}
                    [:x-container {:as "main"}
                     [:div {:data-x-part "main"}]]
                    pages/footer]]]])
   :lists      {:presets      {:part "presets" :row pages/preset}
                :menu-presets {:part "menu-presets" :row pages/preset :of :presets}
                :tags         {:part "tags" :row pages/tag-choice}
                :posts        {:part "posts" :row sidebar-row}
                :pages        {:part "pages" :row pages/page-link}
                :menu-pages   {:part "menu-pages" :row pages/page-link :of :pages}
                :main         {:part "main" :rows pages/page-rows}}})
