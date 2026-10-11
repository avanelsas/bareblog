(ns bareblog.reader
  "Reads the text of a post into blocks. A block and what it holds are data of BareBlog's own:
   nothing in them says how a page shows them."
  (:require [bareblog.header :as header]
            [nextjournal.markdown :as md]))

(defn- plain
  "The text of the Markdown `node`, with nothing of its markup."
  [node]
  (if (= :softbreak (:type node))
    " "
    (or (:text node) (apply str (map plain (:content node))))))

(declare inline)

(defn- children
  "What the Markdown `node` holds, as the content of a block."
  [node]
  (mapv inline (:content node)))

(def ^:private spans
  "The kind of each Markdown node that marks the text it holds and does nothing more."
  {:strong    :strong
   :em        :emphasis
   :monospace :code})

(defn- span [node]
  {:kind (spans (:type node)) :content (children node)})

(defn- link [{:keys [attrs] :as node}]
  (cond-> {:kind :link :href (:href attrs) :content (children node)}
    (:title attrs) (assoc :title (:title attrs))))

(defn- image [{:keys [attrs] :as node}]
  {:kind :image :src (:src attrs) :alt (plain node)})

(defn- space [_node] " ")

(def ^:private inlines
  "How each Markdown node inside a block is read."
  (merge (zipmap (keys spans) (repeat span))
         {:text      :text
          :link      link
          :image     image
          :softbreak space}))

(defn- inline
  "What the Markdown `node` inside a block is: text, or a map with a `:kind`. A node that has
   no entry yet is its plain text."
  [node]
  ((get inlines (:type node) plain) node))

(defn- photo?
  "True when the paragraph `node` holds an image and nothing else."
  [{:keys [content]}]
  (and (= 1 (count content)) (= :image (:type (first content)))))

(defn- photo [node]
  (assoc (image (first (:content node))) :kind :photo))

(defn- text [node]
  {:kind :text :content (children node)})

(defn- paragraph [node]
  (if (photo? node) (photo node) (text node)))

(defn- heading [node]
  {:kind :heading :level (:heading-level node) :content (children node)})

(def ^:private kinds
  "How each Markdown node at the top of a post is read."
  {:paragraph paragraph
   :heading   heading})

(defn- unknown
  "A block that says which Markdown node BareBlog has no kind for yet, with its plain text."
  [node]
  {:kind :unknown :type (:type node) :text (plain node)})

(defn- block [node]
  ((get kinds (:type node) unknown) node))

(defn- title-heading?
  "True when the Markdown `node` is a heading of the first level."
  [node]
  (and (= :heading (:type node)) (= 1 (:heading-level node))))

(defn read-post
  "What the post `text` holds: the fields of its header block, the text of the heading that
   opens it, and its blocks. A heading of the first level that opens the text is not a block."
  [text]
  (let [{:keys [lines body]} (header/split text)
        nodes                (:content (md/parse body))
        opening              (when (title-heading? (first nodes)) (first nodes))]
    {:fields  (header/fields lines)
     :heading (some-> opening plain)
     :blocks  (mapv block (if opening (rest nodes) nodes))}))
