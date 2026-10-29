(ns bareblog.app.blocks
  "The fixed template of each kind of block, and of each kind of thing a block holds."
  (:require [bareblog.app.live :as live]))

(declare child)

(defn- children [content]
  (map child content))

(defn- span [tag {:keys [content]}]
  (into [tag] (children content)))

(defn known
  "The attributes of `attrs` that have a value."
  [attrs]
  (into {} (filter (comp some? val)) attrs))

(defn- element
  "A template of `tag` with the attributes of `attrs` that have a value, and `children`."
  [tag attrs children]
  (let [present (known attrs)]
    (into (if (seq present) [tag present] [tag]) children)))

(defn- link [{:keys [href title rel content]}]
  (element :a {:href href :title title :rel rel} (children content)))

(defn- image [{:keys [src alt width height]}]
  (element :img {:src src :alt alt :width width :height height} []))

(defn- line-break [_]
  [:br])

(defn- note-id [number]
  (str "note-" number))

(defn- mark-id [number]
  (str "note-mark-" number))

(defn- note-content
  "What a block of a note shows beside its mark: the content of a block of text, and nothing of
   a block of another kind."
  [{:keys [content]}]
  (children content))

(defn- sidenote
  "The note `number` beside its mark, of the blocks `blocks`, each after a space. A blog template
   shows it there, or the note among the footnotes."
  [number blocks]
  (into [:span {:class "sidenote" :role "note"} [:span {:class "sidenote-number"} (str number)]]
        (apply concat (interpose [" "] (remove empty? (map note-content blocks))))))

(defn- footnote-mark
  "The mark of a note. Only the first mark of a note holds the note. It has an id, which the note
   links back to, and the note beside it."
  [{:keys [number note]}]
  (let [mark [:sup (element :a
                            {:id (when note (mark-id number)) :href (str "#" (note-id number))}
                            [(str number)])]]
    (if note
      [:span {:class "note-mark"} mark (sidenote number note)]
      mark)))

(defn- unknown-content [{:keys [text]}]
  text)

(def inlines
  "How each kind of thing inside a block becomes a fixed template."
  {:strong        (partial span :strong)
   :emphasis      (partial span :em)
   :code          (partial span :code)
   :strikethrough (partial span :s)
   :link          link
   :image         image
   :break         line-break
   :footnote-mark footnote-mark
   :unknown       unknown-content})

(defn- no-template [what x]
  (ex-info (str "No template for this kind of " what ".") {:kind (:kind x)}))

(defn- child
  "The child of a template that `x` becomes: text stays text."
  [x]
  (if (string? x)
    x
    ((or (inlines (:kind x)) (throw (no-template "content" x))) x)))

(defn- text [{:keys [content]}]
  (into [:p] (children content)))

(defn- heading [{:keys [level content]}]
  (into [(keyword (str "h" level))] (children content)))

(defn ratio
  "The width and the height as `x-image` takes them, or nil when one is not known."
  [{:keys [width height]}]
  (when (and width height)
    (str width ":" height)))

(defn landscape?
  "True when the photo `block` is 3 by 2 or wider."
  [{:keys [width height]}]
  (boolean (and width height (>= (* 2 width) (* 3 height)))))

(defn- size-style
  "The size of the photo `block` as custom properties: its width in pixels, and its width to
   its height. A vector has no width of its own. Nil when its size is not known."
  [{:keys [width height vector?]}]
  (when (and width height)
    (str (when-not vector? (str "--photo-width: " width "px; "))
         "--photo-ratio: " width " / " height)))

(defn- photo [{:keys [src alt caption] :as block}]
  (element :figure
           {:class (when (landscape? block) "landscape") :style (size-style block)}
           (cond-> [(element :x-image {:src src :alt alt :ratio (ratio block)} [])]
             caption (conj [:figcaption caption]))))

(defn- unknown [{:keys [text]}]
  [:p text])

(declare template)

(defn- plain [{:keys [content]}]
  (into [:span] (children content)))

(defn- quote-block [{:keys [blocks]}]
  (into [:blockquote] (map template blocks)))

(defn- item [{:keys [blocks]}]
  (into [:li] (map template blocks)))

(defn- list-block [{:keys [numbered? start items]}]
  (element (if numbered? :ol :ul)
           {:start (when (not= 1 start) start)}
           (map item items)))

(defn- code [{:keys [language text]}]
  (element :x-code {:language language :show-copy true} [text]))

(defn- live [{:keys [text height]}]
  [:div {:class "live"}
   (element :iframe
            {:class   "live-frame"
             :title   "A live example"
             :sandbox "allow-scripts"
             :style   (when height (str "height:" height "px"))
             :srcdoc  (live/document text height)}
            [])
   [:x-code {:language "html" :show-copy true} text]])

(defn- divider [_]
  [:hr])

(def ^:private alignments
  "What `x-table-cell` calls each side that a column is aligned to."
  {:left "start" :center "center" :right "end"})

(defn- cell [attrs {:keys [content align]}]
  (element :x-table-cell (assoc attrs :align (alignments align)) (children content)))

(defn- row [attrs cells]
  (into [:x-table-row] (map (partial cell attrs) cells)))

(defn- table [{:keys [head rows]}]
  (into [:x-table {:columns (str "repeat(" (count head) ", auto)") :bordered true :striped true}
         (row {:type "header" :scope "col"} head)]
        (map (partial row {}) rows)))

(defn- note [{:keys [number blocks]}]
  (-> [:li {:id (note-id number)}]
      (into (map template blocks))
      (conj [:a {:class "note-back" :href (str "#" (mark-id number)) :aria-label "Back to the text"}
             "\u21a9"])))

(defn- footnotes [{:keys [notes]}]
  [:section {:class "footnotes"} (into [:ol] (map note notes))])

(def templates
  "How a block of each kind becomes a fixed template."
  {:text      text
   :plain     plain
   :heading   heading
   :photo     photo
   :quote     quote-block
   :list      list-block
   :code      code
   :live      live
   :divider   divider
   :table     table
   :footnotes footnotes
   :unknown   unknown})

(defn template
  "The fixed template of `block`. Throws when there is none for its kind."
  [block]
  ((or (templates (:kind block)) (throw (no-template "block" block))) block))
