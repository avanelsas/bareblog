(ns bareblog.app.live
  "A live frame: the document that it shows for HTML that an author runs, and the height that
   it tells the page.")

(def ^:private baredom-modules
  "Where a live frame loads the module of each BareDOM component that it shows."
  "https://cdn.jsdelivr.net/npm/@vanelsas/baredom@4.5.0/dist/")

(def height-key
  "The name under which a live frame tells the page the height of what it shows."
  "bareblogLiveHeight")

(def height-request
  "What the page sends a live frame to ask for its height."
  "height")

(def ^:private heights
  "The least and the most height of a live frame that measures itself, in pixels."
  {:least 48 :most 1200})

(def ^:private style
  "How a live frame sets out what it shows: in the middle, with room around it, in the colors
   of the page around it."
  (str ":root{color-scheme:light dark;}html,body{margin:0;background:transparent;}"
       "body{box-sizing:border-box;display:flex;flex-wrap:wrap;align-items:center;"
       "justify-content:center;gap:16px;padding:24px;font-family:system-ui,sans-serif;}"))

(def ^:private fixed-style
  "How a live frame with a height of its own fills that height."
  "body{min-height:100vh;}")

;; A module script runs once the body is parsed, so it finds every element that the HTML of
;; the author makes.
(def ^:private loading
  "The part of the script that registers each BareDOM component in the document. A tag that
   BareDOM has no module for is left as it is."
  (str "const loaded=Promise.allSettled([...new Set([...document.querySelectorAll(\"*\")]"
       ".map(e=>e.localName))].filter(t=>t.startsWith(\"x-\"))"
       ".map(t=>import(\"" baredom-modules "\"+t+\".js\").then(m=>m.init())));"))

;; A frame out of view runs no ResizeObserver, so it also tells its height once it is loaded.
(def ^:private measuring
  "The part of the script that tells the page the height of what a live frame shows: once its
   components are registered, each time it changes, and each time the page asks."
  (str "const tell=()=>parent.postMessage({" height-key
       ":Math.ceil(document.body.getBoundingClientRect().height)},\"*\");"
       "loaded.then(tell);new ResizeObserver(tell).observe(document.body);"
       "addEventListener(\"message\",e=>e.data===\"" height-request "\"&&tell());"))

(defn document
  "The document that a live frame shows for the HTML `text`. With a `height`, the frame keeps
   it, and else it measures itself."
  [text height]
  (str "<!doctype html><html><head><meta charset=\"utf-8\"><style>" style
       (when height fixed-style) "</style></head><body>" text
       "<script type=\"module\">" loading (when-not height measuring) "</script></body></html>"))

(defn fit
  "The height that a live frame takes when it tells the height `told`."
  [told]
  (max (:least heights) (min (:most heights) told)))
