(ns bareblog.app.blocks
  "The fixed template of each kind of block, and of each kind of thing a block holds.")

(declare child)

(defn- children [content]
  (map child content))

(defn- span [tag {:keys [content]}]
  (into [tag] (children content)))

(defn- known
  "The attributes of `attrs` that have a value."
  [attrs]
  (into {} (filter (comp some? val)) attrs))

(defn- link [{:keys [href title content]}]
  (into [:a (known {:href href :title title})] (children content)))

(defn- image [{:keys [src alt width height]}]
  [:img (known {:src src :alt alt :width width :height height})])

(def inlines
  "How each kind of thing inside a block becomes a fixed template."
  {:strong   (partial span :strong)
   :emphasis (partial span :em)
   :code     (partial span :code)
   :link     link
   :image    image})

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

(defn- ratio
  "The width and the height as `x-image` takes them, or nil when one is not known."
  [{:keys [width height]}]
  (when (and width height)
    (str width ":" height)))

(defn- photo [{:keys [src alt] :as block}]
  [:figure [:x-image (known {:src src :alt alt :ratio (ratio block)})]])

(defn- unknown [{:keys [text]}]
  [:p text])

(def templates
  "How a block of each kind becomes a fixed template."
  {:text    text
   :heading heading
   :photo   photo
   :unknown unknown})

(defn template
  "The fixed template of `block`. Throws when there is none for its kind."
  [block]
  ((or (templates (:kind block)) (throw (no-template "block" block))) block))
