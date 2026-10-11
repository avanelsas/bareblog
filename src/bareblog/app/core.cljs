(ns bareblog.app.core
  "Starts the blog app in a page."
  (:require [bareblog.address :as address]
            [bareblog.app.render :as render]
            [bareblog.app.templates :as templates]
            [bareblog.app.view :as view]
            [bareblog.site :as site]
            [bareblog.wire :as wire]
            [baredom.exports.x-badge :as x-badge]
            [baredom.exports.x-code :as x-code]
            [baredom.exports.x-container :as x-container]
            [baredom.exports.x-image :as x-image]
            [baredom.exports.x-navbar :as x-navbar]
            [baredom.exports.x-pagination :as x-pagination]
            [baredom.exports.x-table :as x-table]
            [baredom.exports.x-table-cell :as x-table-cell]
            [baredom.exports.x-table-row :as x-table-row]
            [baredom.exports.x-theme :as x-theme]
            [baremirror.alpha.events :as events]
            [cljs.reader :as reader]))

(def ^:private registrations
  "The function that registers each custom element a blog template can name."
  {"x-badge"      x-badge/init
   "x-code"       x-code/init
   "x-container"  x-container/init
   "x-image"      x-image/init
   "x-navbar"     x-navbar/init
   "x-pagination" x-pagination/init
   "x-table"      x-table/init
   "x-table-cell" x-table-cell/init
   "x-table-row"  x-table-row/init
   "x-theme"      x-theme/init})

(def ^:private loading
  "The attribute that `index.html` gives the document until the page shows its data. BareDOM's
   prerender script waits for it to go."
  "data-loading")

(def ^:private shown-mark
  "The name of the mark that the page leaves on its timeline each time it has shown its data."
  "bareblog-shown")

(defn- register! [tag]
  (if-some [init (registrations tag)]
    (init)
    (throw (ex-info "A blog template names a custom element that the app does not register."
                    {:tag tag}))))

(defn- register-components!
  "Registers the custom elements that the page view `page` puts in the page."
  [page]
  (run! register! (templates/custom-elements (templates/named (:template page)) page)))

(defn- show-page!
  "Shows the page that `told` describes, of the blog in `site`."
  [site told]
  (let [page (view/page site told)]
    (register-components! page)
    (render/render! js/document page)
    (.removeAttribute (.-documentElement js/document) loading)
    (js/performance.mark shown-mark)))

(defn- show!
  "Shows what the page is told in `told`: the site and the page, or a problem in place of them.
   Something that is wrong with the blog is shown above the page."
  [{:keys [site page problem]}]
  (render/notice! js/document (or problem (site/notice site)))
  (when-not problem
    (show-page! site page)))

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

(def ^:private no-page
  "What a page is told when the server has no data for its address."
  (pr-str {:page {:kind :not-found}}))

(defn- response-text
  "The text of `response`. A response that says there is no such file gives `missing`."
  [missing ^js response]
  (cond
    (.-ok response)                            (.text response)
    (and missing (= 404 (.-status response))) missing
    :else                                      (throw (js/Error. (str "The server answered "
                                                                      (.-status response) " for "
                                                                      (.-url response) ".")))))

(defn- fetch-data
  "A promise of the data at `address`. With `missing`, that is what an address with no file
   gives."
  [address missing]
  (-> (js/fetch address)
      (.then (partial response-text missing))
      (.then reader/read-string)))

(defn- to-page!
  "Goes to page `number` of the home page."
  [number]
  (set! (.-href js/location) (address/home number)))

(def ^:private actions
  "What the app does for each meaning of a message."
  {:to-page to-page!})

(defn- dispatch! [[meaning argument]]
  (if-some [act! (actions meaning)]
    (act! argument)
    (throw (ex-info "The app has no action for this message." {:meaning meaning}))))

(defn- asked-page [_origin ^js event]
  (.. event -detail -page))

(def ^:private arguments
  "What gives the argument of a message, for each meaning that has one."
  {:to-page asked-page})

(defn- with-argument [meaning]
  [meaning (arguments meaning)])

(defn- all-events
  "The event meanings of every blog template, each with what gives its argument."
  []
  (update-vals (into {} (map templates/events) (vals templates/by-name)) with-argument))

(defonce ^{:private true :doc "True once the events of the document are turned into messages."}
  listening?
  (atom false))

(defn- listen-for-events!
  "Turns the events that the blog templates name into messages, once."
  []
  (when (compare-and-set! listening? false true)
    (events/listen! (.-body js/document) {:dispatch! dispatch! :events (all-events)})))

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
  "Opens the socket at `changes`, unless one is open, and shows what it sends."
  [changes]
  (when (nil? @socket)
    (doto (reset! socket (js/WebSocket. (socket-address changes)))
      (.addEventListener "message" #(show-message! %))
      (.addEventListener "close" (partial listen-again! changes)))))

(defn- show-with-site!
  "Fetches the data of the site and shows `page` with it."
  [page]
  (-> (fetch-data address/site-data nil)
      (.then (fn [site] (show-or-report! (site/told site page))))))

;; A page whose data names a socket is told everything over it, when it opens and after each
;; change. Any other page fetches the data of the site once.
(defn- start!
  "Shows the page that `page` describes, and follows its files when it has a socket."
  [{:keys [changes] :as page}]
  (listen-for-events!)
  (if changes
    (listen! changes)
    (show-with-site! page)))

(defn ^:dev/after-load load!
  "Fetches the data of this page, shows the page, and listens for it to change."
  []
  (-> (fetch-data wire/page-data no-page)
      (.then start!)
      (.catch report!)))

(defn init []
  (load!))
