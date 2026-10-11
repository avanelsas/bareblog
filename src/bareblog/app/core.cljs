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

(def ^:private loading
  "The attribute that `index.html` gives the document until the page shows its data. BareDOM's
   prerender script waits for it to go."
  "data-loading")

(defn- register! [tag]
  (if-some [init (registrations tag)]
    (init)
    (throw (ex-info "A blog template names a custom element that the app does not register."
                    {:tag tag}))))

(defn- register-components!
  "Registers the custom elements that the blog template of `page` can put in a page."
  [page]
  (run! register! (templates/custom-elements (templates/named (:template page)))))

(defn- show-page!
  "Shows the page of `post` in the blog `blog`."
  [blog post]
  (let [page (view/post-page blog post)]
    (register-components! page)
    (render/render! js/document page)
    (.removeAttribute (.-documentElement js/document) loading)))

(defn- show!
  "Shows what the page is told in `told`: its data, or a problem in place of them."
  [{:keys [blog post problem]}]
  (render/notice! js/document problem)
  (when-not problem
    (show-page! blog post)))

(defn- report!
  "Shows `error` in the page, and gives it to the console."
  [error]
  (js/console.error error)
  (render/notice! js/document (str "This page could not be shown. " (ex-message error))))

(defn- show-or-report! [told]
  (try
    (show! told)
    (catch :default error
      (report! error))))

(defn- response-text [^js response]
  (if (.-ok response)
    (.text response)
    (throw (js/Error. (str "The server has no data for this page (" (.-status response) ").")))))

(defonce ^{:private true :doc "The socket that tells this page what to show, once it is open."}
  socket
  (atom nil))

(def ^:private try-again
  "How long after a socket closed the page asks for another, in milliseconds."
  1000)

(defn- socket-address
  "The address of the socket at `changes`, which is an address beside this page."
  [changes]
  (let [^js url (js/URL. changes (.-href js/location))]
    (set! (.-protocol url) (if (= "https:" (.-protocol js/location)) "wss:" "ws:"))
    (.-href url)))

(defn- show-message! [^js message]
  (show-or-report! (reader/read-string (.-data message))))

(declare listen!)

(defn- listen-again! [changes]
  (reset! socket nil)
  (js/setTimeout (partial listen! changes) try-again))

;; The socket outlives a reload of the code in development. It calls `show-message!` by name
;; each time, so that it calls the function of the code that is loaded now.
(defn- listen!
  "Opens the socket at `changes`, unless one is open, and shows what it sends. A page whose
   data names no socket opens none."
  [changes]
  (when (and changes (nil? @socket))
    (doto (reset! socket (js/WebSocket. (socket-address changes)))
      (.addEventListener "message" #(show-message! %))
      (.addEventListener "close" (partial listen-again! changes)))))

(defn- start!
  "Shows what the page is told in `told`, and listens for it to change."
  [told]
  (show-or-report! told)
  (listen! (:changes told)))

(defn ^:dev/after-load load!
  "Fetches the data of this page, shows it, and listens for it to change."
  []
  (-> (js/fetch wire/page-data)
      (.then response-text)
      (.then reader/read-string)
      (.then start!)
      (.catch report!)))

(defn init []
  (load!))
