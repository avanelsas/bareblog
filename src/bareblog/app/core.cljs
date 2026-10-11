(ns bareblog.app.core
  "Starts the blog app in a page."
  (:require [bareblog.app.render :as render]
            [bareblog.app.templates :as templates]
            [bareblog.app.view :as view]
            [bareblog.wire :as wire]
            [baredom.exports.x-container :as x-container]
            [baredom.exports.x-image :as x-image]
            [baredom.exports.x-navbar :as x-navbar]
            [baredom.exports.x-theme :as x-theme]
            [cljs.reader :as reader]))

(def ^:private registrations
  "The function that registers each custom element a blog template can name."
  {"x-container" x-container/init
   "x-image"     x-image/init
   "x-navbar"    x-navbar/init
   "x-theme"     x-theme/init})

(defn- register! [tag]
  (if-some [init (registrations tag)]
    (init)
    (throw (ex-info "A blog template names a custom element that the app does not register."
                    {:tag tag}))))

(defn- register-components!
  "Registers the custom elements that the blog template of `page` can put in a page."
  [page]
  (run! register! (templates/custom-elements (templates/named (:template page)))))

(defn- show!
  "Shows the page that the EDN `text` describes."
  [text]
  (let [{:keys [blog post]} (reader/read-string text)
        page                (view/post-page blog post)]
    (register-components! page)
    (set! (.-title js/document) (:title page))
    (render/render! (.-body js/document) page)))

(defn- response-text [^js response]
  (if (.-ok response)
    (.text response)
    (throw (js/Error. (str "No data for this page: " (.-status response))))))

(defn- report! [error]
  (js/console.error error))

(defn ^:dev/after-load load!
  "Fetches the data of this page and shows it."
  []
  (-> (js/fetch wire/page-data)
      (.then response-text)
      (.then show!)
      (.catch report!)))

(defn init []
  (load!))
