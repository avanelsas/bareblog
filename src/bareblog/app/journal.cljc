(ns bareblog.app.journal
  "The Journal blog template: one reading column under a navbar.

   See `bareblog.app.templates` for what a blog template is."
  (:require [bareblog.app.pages :as pages]
            [baremirror.alpha.template :as template]))

;; Below its breakpoint `x-navbar` shows its brand and its toggle and nothing else. The links
;; and the controls are then in a drawer, which the toggle opens. Both places show the same
;; state, so neither has to be kept in step with the other.
(def template
  "The node of the whole document. Its lists hold the node of the page, by its key, and the
   presets of the theme select, once in the navbar and once in the menu."
  {:key        "journal"
   :title      pages/title
   :home       :entries
   :choice     :home
   :search     :field
   :split      (template/split
                :journal
                [:x-theme {:preset :preset}
                 [:div {:class "page journal"}
                  (pages/navbar pages/search-field)
                  [:x-drawer {:label (:menu pages/words) :placement "right" :open :menu?
                              :on    {"x-drawer-toggle" :menu}}
                   [:div {:slot "header" :class "menu-header"}
                    [:span (:menu pages/words)]
                    pages/close-button]
                   (-> [:div {:class "menu"}]
                       (into (pages/links "menu-pages"))
                       (conj pages/search-field (pages/theme-select "menu-presets")))]
                  [:x-container {:as "main"}
                   [:div {:data-x-part "main"}]]]])
   :lists      {:presets      {:part "presets" :row pages/preset}
                :menu-presets {:part "menu-presets" :row pages/preset :of :presets}
                :pages        {:part "pages" :row pages/page-link}
                :menu-pages   {:part "menu-pages" :row pages/page-link :of :pages}
                :main         {:part "main" :rows pages/page-rows}}})
