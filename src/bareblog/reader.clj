(ns bareblog.reader
  "Reads the text of a post into blocks. A block and what it holds are data of BareBlog's own:
   nothing in them says how a page shows them.

   A block is a map with a `:kind`. A text block holds content: text, or a map with a `:kind`
   and its own content. A quote, an item of a list and a footnote hold blocks.

       {:kind :text :content [\"We took the \" {:kind :strong :content [\"tram 28\"]} \".\"]}

   The kinds of block are a closed list: text, heading, photo, quote, list, code, divider, table
   and footnotes, and plain, which is the text of a list item that has no paragraph around it.
   A Markdown node with no kind becomes `:unknown`, as a block or inside one, and keeps its
   type.

   HTML that an author types outside a code fence is shown as code. A comment is the
   exception: it is left out, since an author writes one so that it does not show."
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
  "What the Markdown `node` holds, as the content of a block. A node that is read as nothing
   is left out."
  [node]
  (into [] (keep inline) (:content node)))

(def ^:private spans
  "The kind of each Markdown node that marks the text it holds and does nothing more."
  {:strong        :strong
   :em            :emphasis
   :monospace     :code
   :strikethrough :strikethrough})

(defn- span [node]
  {:kind (spans (:type node)) :content (children node)})

(defn- link [{:keys [attrs] :as node}]
  (cond-> {:kind :link :href (:href attrs) :content (children node)}
    (:title attrs) (assoc :title (:title attrs))))

(defn- image [{:keys [attrs] :as node}]
  {:kind :image :src (:src attrs) :alt (plain node)})

(defn- space [_node] " ")

(defn- line-break [_node]
  {:kind :break})

(defn- footnote-mark
  "The mark of a footnote in the text, with the number of the note."
  [{:keys [ref]}]
  {:kind :footnote-mark :number (inc ref)})

(defn- comment?
  "True when the HTML `text` is comments and nothing else."
  [text]
  (some? (re-matches #"(?s)\s*(<!--.*?-->\s*)+" text)))

(defn- html-as-code
  "HTML that was typed into a sentence, as the code it is. A comment is nothing."
  [node]
  (when-not (comment? (plain node))
    {:kind :code :content [(plain node)]}))

(defn- unknown-content
  "What says which Markdown node inside a block BareBlog has no kind for yet, with its plain
   text."
  [node]
  {:kind :unknown :type (:type node) :text (plain node)})

(def ^:private inlines
  "How each Markdown node inside a block is read."
  (merge (zipmap (keys spans) (repeat span))
         {:text         :text
          :link         link
          :image        image
          :softbreak    space
          :hardbreak    line-break
          :footnote-ref footnote-mark
          :html-inline  html-as-code}))

(defn- inline
  "What the Markdown `node` inside a block is: text, a map with a `:kind`, or nothing."
  [node]
  ((get inlines (:type node) unknown-content) node))

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

(declare block)

(defn- blocks-of
  "The blocks that the Markdown `nodes` are. A node that is read as nothing is left out."
  [nodes]
  (into [] (keep block) nodes))

(defn- blocks
  "The blocks that the Markdown `node` holds."
  [node]
  (blocks-of (:content node)))

(defn- plain-text
  "Text that stands in a list item with no paragraph around it."
  [node]
  {:kind :plain :content (children node)})

(defn- quote-block [node]
  {:kind :quote :blocks (blocks node)})

(defn- item [node]
  {:blocks (blocks node)})

(defn- bullet-list [node]
  {:kind :list :items (mapv item (:content node))})

(defn- numbered-list [node]
  (assoc (bullet-list node)
         :numbered? true
         :start     (get-in node [:attrs :start] 1)))

(defn- code [node]
  (cond-> {:kind :code :text (plain node)}
    (:language node) (assoc :language (:language node))))

(defn- html-block
  "HTML that was typed outside a code fence, as the code it is. A comment is nothing."
  [node]
  (when-not (comment? (plain node))
    {:kind :code :language "html" :text (plain node)}))

(defn- divider [_node]
  {:kind :divider})

(defn- cell [node]
  (cond-> {:content (children node)}
    (:alignment node) (assoc :align (:alignment node))))

(defn- row [node]
  (mapv cell (:content node)))

(defn- rows-of
  "The rows of the part of the table `node` that has the type `part`."
  [node part]
  (into [] (comp (filter (comp #{part} :type)) (mapcat :content) (map row)) (:content node)))

(defn- table [node]
  {:kind :table
   :head (first (rows-of node :table-head))
   :rows (rows-of node :table-body)})

(def ^:private kinds
  "How each Markdown node that is a block is read."
  {:paragraph     paragraph
   :plain         plain-text
   :heading       heading
   :blockquote    quote-block
   :bullet-list   bullet-list
   :numbered-list numbered-list
   :code          code
   :html-block    html-block
   :ruler         divider
   :table         table})

(defn- unknown
  "A block that says which Markdown node BareBlog has no kind for yet, with its plain text."
  [node]
  {:kind :unknown :type (:type node) :text (plain node)})

(defn- block
  "The block that the Markdown `node` is, or nothing."
  [node]
  ((get kinds (:type node) unknown) node))

(defn- title-heading?
  "True when the Markdown `node` is a heading of the first level."
  [node]
  (and (= :heading (:type node)) (= 1 (:heading-level node))))

(defn- note [{:keys [ref] :as node}]
  {:number (inc ref) :blocks (blocks node)})

(defn- footnotes
  "The block that holds the footnotes `nodes` in the order of their numbers, in a vector. No
   footnotes is no block."
  [nodes]
  (if (seq nodes)
    [{:kind :footnotes :notes (vec (sort-by :number (map note nodes)))}]
    []))

(defn- footnote-mark? [x]
  (and (map? x) (= :footnote-mark (:kind x))))

(defn- mark-paths
  "The path of each footnote mark in `x`, in the order of the text. `path` is where `x` is."
  [path x]
  (cond
    (footnote-mark? x) [path]
    (map? x)           (mapcat (fn [[k v]] (mark-paths (conj path k) v)) x)
    (vector? x)        (mapcat (fn [i v] (mark-paths (conj path i) v)) (range) x)
    :else              []))

(defn- keep-first
  "The `firsts` with `path` for its note, unless the note has a path."
  [blocks firsts path]
  (merge {(:number (get-in blocks path)) path} firsts))

(defn- first-of-its-note
  "The paths of `paths`, which are in the order of the text, that lead to the first mark of a
   note in `blocks`."
  [blocks paths]
  (vals (reduce (partial keep-first blocks) {} paths)))

(defn- mark-first [blocks path]
  (assoc-in blocks (conj path :first?) true))

(defn- with-first-marks
  "The `blocks` with `:first? true` in the first mark of each note. A note links back to it."
  [blocks]
  (reduce mark-first
          blocks
          (first-of-its-note blocks (mark-paths [] blocks))))

(defn read-post
  "What the post `text` holds: the fields of its header block and what is wrong with it, the
   text of the heading that opens it, and its blocks. A heading of the first level that opens
   the text is not a block. The footnotes are the last block."
  [text]
  (let [{:keys [lines body]} (header/split text)
        doc                  (md/parse body)
        nodes                (:content doc)
        opening              (when (title-heading? (first nodes)) (first nodes))]
    {:fields   (header/fields lines)
     :problems (header/problems lines)
     :heading  (some-> opening plain)
     :blocks   (with-first-marks
                (into (blocks-of (if opening (rest nodes) nodes))
                      (footnotes (:footnotes doc))))}))
