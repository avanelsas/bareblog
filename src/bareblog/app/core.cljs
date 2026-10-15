(ns bareblog.app.core
  "Starts the blog app in a page.

   The edge of the app. The state is in one atom. A message goes through `state/step`, and
   `state/shown` says what the page then shows. This namespace brings the document, the address
   and the browser's storage to that, and turns events and what the socket says into messages."
  (:require [bareblog.app.pages :as pages]
            [bareblog.app.render :as render]
            [bareblog.app.state :as state]
            [bareblog.app.templates :as templates]
            [bareblog.blog.address :as address]
            [bareblog.blog.site :as site]
            [bareblog.blog.wire :as wire]
            [baredom.exports.x-badge :as x-badge]
            [baredom.exports.x-code :as x-code]
            [baredom.exports.x-command-palette :as x-command-palette]
            [baredom.exports.x-container :as x-container]
            [baredom.exports.x-drawer :as x-drawer]
            [baredom.exports.x-gaussian-blur :as x-gaussian-blur]
            [baredom.exports.x-image :as x-image]
            [baredom.exports.x-navbar :as x-navbar]
            [baredom.exports.x-pagination :as x-pagination]
            [baredom.exports.x-search-field :as x-search-field]
            [baredom.exports.x-select :as x-select]
            [baredom.exports.x-sidebar :as x-sidebar]
            [baredom.exports.x-table :as x-table]
            [baredom.exports.x-table-cell :as x-table-cell]
            [baredom.exports.x-table-row :as x-table-row]
            [baredom.exports.x-theme :as x-theme]
            [baredom.requests :as requests]
            [baremirror.alpha.events :as events]
            [cljs.reader :as reader]))

(def ^:private registrations
  "The function that registers each custom element a blog template can name."
  {"x-badge"        x-badge/init
   "x-code"            x-code/init
   "x-command-palette" x-command-palette/init
   "x-container"    x-container/init
   "x-drawer"       x-drawer/init
   "x-gaussian-blur" x-gaussian-blur/init
   "x-image"        x-image/init
   "x-navbar"       x-navbar/init
   "x-pagination"   x-pagination/init
   "x-search-field" x-search-field/init
   "x-select"          x-select/init
   "x-sidebar"         x-sidebar/init
   "x-table"        x-table/init
   "x-table-cell"   x-table-cell/init
   "x-table-row"    x-table-row/init
   "x-theme"        x-theme/init})

(def ^:private loading
  "The attribute that `index.html` gives the document until the page shows its data. BareDOM's
   prerender script waits for it to go."
  "data-loading")

(def ^:private shown-mark
  "The name of the mark that the page leaves on its timeline each time it has shown its data."
  "bareblog-shown")

(def ^:private preset-key
  "The name under which the browser keeps the preset that the reader chose."
  "bareblog-preset")

(defn- register! [tag]
  (if-some [init (registrations tag)]
    (init)
    (throw (ex-info "A blog template names a custom element that the app does not register."
                    {:tag tag}))))

(defn- register-components!
  "Registers the custom elements that the page view `view` puts in the page."
  [view]
  (run! register! (templates/custom-elements (templates/named (:template view)) view)))

;; Bringing what is outside the app to what the state shows. Each writes only where it differs.

(defn- show-view!
  "Brings the document to the page view `view`."
  [view]
  (register-components! view)
  (render/render! js/document view)
  (.removeAttribute (.-documentElement js/document) loading)
  (js/performance.mark shown-mark))

(defn- set-param!
  "Makes `text` what `params` holds under `k`. No text takes the name away."
  [^js params [k text]]
  (if (seq text)
    (.set params (name k) text)
    (.delete params (name k))))

(defn- set-asked!
  "Makes the address ask for `asked`, a map from a name to text, without a new entry in the
   history. Whatever else the address asks for, and what follows its hash, stays."
  [asked]
  (let [^js url (js/URL. (.-href js/location))]
    (run! (partial set-param! (.-searchParams url)) asked)
    (when (not= (.-href url) (.-href js/location))
      (js/history.replaceState nil "" (.-href url)))))

(defn- read-stored
  "What the browser keeps under `k`, or nil when it keeps nothing or lets nobody read."
  [k]
  (try
    (.getItem js/localStorage k)
    (catch :default _ nil)))

(defn- store!
  "Makes the browser keep `text` under `k`, where it differs. A browser that keeps nothing is
   left alone."
  [k text]
  (try
    (when (and text (not= text (.getItem js/localStorage k)))
      (.setItem js/localStorage k text))
    (catch :default _ nil)))

(defn- show!
  "Brings the page to what `state/shown` gave: the notice, the document, the address and the
   preset that the browser keeps."
  [{:keys [notice view asked preset]}]
  (render/notice! js/document notice)
  (when view
    (show-view! view))
  (when asked
    (set-asked! asked))
  (store! preset-key preset))

(defn- report!
  "Shows `error` in the page, and gives it to the console."
  [error]
  (js/console.error error)
  (render/notice! js/document (str "This page could not be shown. " (ex-message error))))

(defn- show-or-report! [shown]
  (try
    (show! shown)
    (catch :default error
      (report! error))))

;; Messages.

(defn- asked-for
  "What the address of this page asks for, by name."
  []
  (let [params (js/URLSearchParams. (.-search js/location))]
    {:query (.get params "q") :tag (.get params "tag")}))

(defn- document-root
  "Where the top of the blog is, as the document says."
  []
  (.getAttribute (.-documentElement js/document) wire/root-attribute))

(defonce ^{:private true :doc "The state of this page."}
  page-state
  (atom (state/initial {})))

(def ^:private step-and-show!
  "A function of a message: it puts the state after the message in the atom, and shows it."
  (events/dispatcher page-state state/step state/shown show-or-report!))

(defn- changes?
  "True when `message` changes the state of this page."
  [message]
  (not= @page-state (state/step @page-state message)))

(defn- dispatch!
  "Takes the reader where `message` leads, or else lets it change the state. A message that
   does neither is dropped."
  [message]
  (if-some [to (state/destination @page-state message)]
    (set! (.-href js/location) to)
    (when (changes? message)
      (step-and-show! message))))

(defn- detail-page [_origin ^js event]
  (.. event -detail -page))

(defn- detail-value [_origin ^js event]
  (.. event -detail -value))

(defn- detail-open [_origin ^js event]
  (.. event -detail -open))

(defn- click-facts
  "What `state/link-picks?` asks of a click."
  [^js event]
  {:button    (.-button event)
   :modified? (or (.-metaKey event) (.-ctrlKey event) (.-shiftKey event) (.-altKey event))})

(defn- row-key
  "The key of the row that the event came from."
  [origin _event]
  (peek (:key-path origin)))

(defn- tag-picked!
  "The tag that a click on a link to it picks in place, or nil when the click follows the
   link. A click that picks in place is stopped from following the link."
  [origin ^js event]
  (when (state/link-picks? @page-state (click-facts event))
    (.preventDefault event)
    (row-key origin event)))

(defn- detail-address
  "The address that the command picked in a palette leads to."
  [_origin ^js event]
  (.. event -detail -item -value))

(defn- nothing [_origin _event]
  nil)

(def ^:private arguments
  "What gives the argument of a message, for each meaning."
  {:to-page       detail-page
   :search        detail-value
   :submit-search nothing
   :preset        detail-value
   :tag           row-key
   :tag-link      tag-picked!
   :clear         nothing
   :menu          detail-open
   :open-menu     nothing
   :close-menu    nothing
   :open-palette  nothing
   :close-palette nothing
   :open-post     detail-address})

(defn- with-argument [meaning]
  [meaning (arguments meaning)])

(defn- all-events
  "The event meanings of every blog template, each with what gives its argument."
  []
  (update-vals (into {} (map templates/events) (vals templates/by-name)) with-argument))

;; What the page knows of its keyboard, and the key that opens the palette.

(defn- platform
  "The name of what the browser runs on, by the newer way of asking where there is one."
  []
  (str (or (some-> (unchecked-get js/navigator "userAgentData") (unchecked-get "platform"))
           (unchecked-get js/navigator "platform"))))

(defn- command-key?
  "True when the keyboard of the reader has a Command key."
  []
  (boolean (re-find #"(?i)mac|iphone|ipad|ios" (platform))))

(defn- palette-key?
  "True when `event` is the key that opens the palette: K with Command or Control held."
  [^js event]
  (and (= "k" (.toLowerCase (str (.-key event))))
       (or (.-metaKey event) (.-ctrlKey event))))

(defn- open-palette-by-key!
  "Opens the palette for its key, where the blog template has one."
  [^js event]
  (when (and (palette-key? event) (= :palette (:search (state/blog-shape @page-state))))
    (.preventDefault event)
    (dispatch! [:open-palette])))

;; A menu is for a narrow window. One that is open when the window becomes wide would lie
;; over a page that shows its links, and would be open again when the window becomes narrow.
(defn- close-menu-with-window!
  "Closes the menu whenever the window becomes wide or narrow."
  []
  (.addEventListener (js/matchMedia (str "(min-width: " pages/wide-from "px)"))
                     "change"
                     (fn [_event] (dispatch! [:close-menu]))))

(defn- listen-to-window!
  "Listens for the key that opens the palette, and for the window becoming wide or narrow."
  []
  (.addEventListener js/document "keydown" #(open-palette-by-key! %))
  (close-menu-with-window!))

(defonce ^{:private true :doc "True once the events of the document are turned into messages."}
  listening?
  (atom false))

(defn- listen-for-events!
  "Turns the events that the blog templates name into messages, once."
  []
  (when (compare-and-set! listening? false true)
    (listen-to-window!)
    (events/listen! (.-body js/document)
                    {:dispatch! dispatch!
                     :events    (all-events)
                     :requests  requests/requests})))

;; What the page is told: by its files, or over its socket.

(def ^:private no-page
  "What a page is told when the server has no data for its address."
  {:page {:kind :not-found}})

(defn- response-data
  "A promise of the data in `response`. A response that says there is no such file gives
   `missing`, when there is one."
  [missing ^js response]
  (cond
    (.-ok response)                            (.then (.text response) reader/read-string)
    (and missing (= 404 (.-status response))) missing
    :else                                      (throw (js/Error. (str "The server answered "
                                                                      (.-status response) " for "
                                                                      (.-url response) ".")))))

;; A host lets a browser keep a file for a while, and a page that was published since is then
;; shown with the data of the blog before it: what the publish added goes from the page again.
;; So the browser is told to ask the host each time whether the data it keeps is still the
;; data. A host that has nothing new answers with no more than that.
(def ^:private asked-anew
  "How data is fetched: what the browser keeps is used only when the host says it still holds."
  #js {:cache "no-cache"})

(defn- fetch-data
  "A promise of the data at `address`, as the host has it now. With `missing`, that is what
   an address with no file gives."
  [address missing]
  (.then (js/fetch address asked-anew) (partial response-data missing)))

(defn- missing?
  "True when the document says that it is the page for an address with no page."
  []
  (.hasAttribute (.-documentElement js/document) wire/missing-attribute))

;; The page for an address with no page is sent for any address, so the data beside it would
;; be that of another page, or of none. It asks for none.
(defn- fetch-page
  "A promise of the data of this page."
  []
  (if (missing?)
    (js/Promise.resolve no-page)
    (fetch-data wire/page-data no-page)))

(defn- told! [told]
  (dispatch! [:told told]))

(defonce ^{:private true :doc "The socket that tells this page what to show, once it is open."}
  socket
  (atom nil))

(def ^:private try-again
  "How long after a socket closed the page asks for another, in milliseconds."
  1000)

(defn- socket-address
  "The address of the socket at `changes`, which is an address beside this page."
  [changes]
  (let [^js url (js/URL. changes (str (.-origin js/location) (.-pathname js/location)))]
    (set! (.-protocol url) (if (= "https:" (.-protocol js/location)) "wss:" "ws:"))
    (.-href url)))

(defn- told-by-socket! [^js message]
  (told! (reader/read-string (.-data message))))

(declare listen!)

(defn- listen-again! [changes]
  (reset! socket nil)
  (js/setTimeout (partial listen! changes) try-again))

;; The socket outlives a reload of the code in development. It calls `told-by-socket!` by name
;; each time, so that it calls the function of the code that is loaded now.
(defn- listen!
  "Opens the socket at `changes`, unless one is open, and takes what it sends as what the page
   is told."
  [changes]
  (when (nil? @socket)
    (doto (reset! socket (js/WebSocket. (socket-address changes)))
      (.addEventListener "message" #(told-by-socket! %))
      (.addEventListener "close" (partial listen-again! changes)))))

(defn- tell-with-site!
  "Fetches the data of the site, and tells the page that and `page`."
  [page]
  (-> (fetch-data (address/site-data (:root @page-state)) nil)
      (.then (fn [site] (told! (site/told site page))))))

;; A page whose data names a socket is told everything over it, when it opens and after each
;; change. Any other page fetches the data of the site once.
(defn- start!
  "Tells the page what `page` describes, and follows its files when it has a socket."
  [{:keys [changes] :as page}]
  (listen-for-events!)
  (if changes
    (listen! changes)
    (tell-with-site! page)))

;; After a reload of the code in development, the page shows its state again with the new code.
(defn ^:dev/after-load reload! []
  (show-or-report! (state/shown @page-state)))

(defn init
  "Takes what the reader chose before from the address and the browser, fetches the data of
   this page, and starts."
  []
  (reset! page-state (state/initial (assoc (asked-for)
                                           :root         (document-root)
                                           :preset       (read-stored preset-key)
                                           :command-key? (command-key?))))
  (-> (fetch-page)
      (.then start!)
      (.catch report!)))
