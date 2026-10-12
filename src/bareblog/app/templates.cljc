(ns bareblog.app.templates
  "The blog templates, by the name a blog gives in its settings, and what can be read from one.

   A blog template is a tree of nodes. A node has a template: `:split`, with a hole for each
   thing the view gives it, or `:fixed-of`, a function from what the view gives it to a fixed
   template. A node may have `:lists`. A list names the part that holds its rows, and says what
   node a row is: `:row` for every row, or `:rows` by the key of the row. A list with `:of`
   shows the rows that the view gives under that name. A node may have `:props`: by part, the
   properties that the renderer sets, each with what gives its value.

   The node of the document also says three things of the blog template. `:home` is what its
   home page shows, `:entries` or `:newest-post`. `:choice` is where what a reader chose takes
   effect, `:home` or `:everywhere`. `:search` is how a reader searches, with a `:field` or a
   `:palette`. The view, the state and the server read these, and none of them names a blog
   template. The nodes of the pages are in `pages`, and every blog template uses them.

   The view of a page is the same tree, filled in: an item for each node, and the keys and the
   rows of each list. One function in `render` brings any node to its view."
  (:require [bareblog.app.journal :as journal]
            [bareblog.app.notebook :as notebook]
            [bareblog.site :as site]
            [clojure.string :as str]))

(def default :journal)

(def by-name
  {:journal  journal/template
   :notebook notebook/template})

(defn named
  "The blog template with the name `template-name`. Throws when there is none."
  [template-name]
  (or (by-name template-name)
      (throw (ex-info "No blog template has this name." {:template template-name}))))

(defn name-of
  "The name of the blog template that the settings `blog` name, or of the default one when
   they name none."
  [blog]
  (:template blog default))

(defn of-blog
  "The blog template of the blog with the settings `blog`."
  [blog]
  (named (name-of blog)))

(def ^:private shows-choice
  "The kinds of page that show what a reader chose, for each place that a blog template names."
  {:home       #{:home}
   :everywhere some?})

(defn chooses-on?
  "True when a page of the kind `kind` shows what a reader chose of the posts, in the blog
   template `template`."
  [template kind]
  (boolean ((shows-choice (:choice template)) kind)))

(defn home-pages
  "How many pages the home page of the blog in `site` has in the blog template `template`: as
   many as its entries fill, or one where it shows a post."
  [template site]
  (if (= :entries (:home template))
    (site/page-count site)
    1))

(defn list-names
  "The names of the lists that the view gives the document of the blog template `template`."
  [template]
  (into #{} (map (fn [[list-name row-list]] (:of row-list list-name))) (:lists template)))

(defn row-node
  "The node of the row with the key `k` in the list `row-list` of a blog template."
  [{:keys [row rows]} k]
  (or row
      (get rows k)
      (throw (ex-info "The blog template has no node for this row." {:key k}))))

(defn rows-of
  "What the view gives the list `row-list`, which has the name `list-name` in its node, from
   `shown`, which is what the view gives that node. A list with `:of` shows the rows that the
   view gives under that name, so that two lists can show the same rows."
  [row-list list-name shown]
  (get-in shown [:lists (:of row-list list-name)]))

(declare fixed-templates)

(defn- list-templates
  "The fixed templates in the rows that `shown` gives for the list `row-list`."
  [row-list {:keys [order by-key]}]
  (mapcat (fn [k] (fixed-templates (row-node row-list k) (by-key k))) order))

(defn fixed-templates
  "Every fixed template that the node `node` of a blog template puts in a page for `shown`,
   which is what the view gives that node."
  [{:keys [split fixed-of lists]} shown]
  (cons (if split (:fixed split) (fixed-of shown))
        (mapcat (fn [[list-name row-list]] (list-templates row-list (rows-of row-list list-name shown)))
                lists)))

(defn- tag-names
  "The tag of every element in the fixed template `fixed`."
  [fixed]
  (map (comp name first) (filter vector? (tree-seq vector? seq fixed))))

(defn- custom? [tag-name]
  (str/includes? tag-name "-"))

(defn custom-elements
  "The tags of the custom elements that the blog template `template` puts in a page for the
   view `view`."
  [template view]
  (into (sorted-set) (comp (mapcat tag-names) (filter custom?)) (fixed-templates template view)))

(defn nodes
  "The node `node` of a blog template and every node below it."
  [{:keys [lists] :as node}]
  (cons node (mapcat nodes (mapcat (fn [{:keys [row rows]}] (if row [row] (vals rows))) (vals lists)))))

(defn events
  "The event meanings of every template in the blog template `template`, by event type and
   part name."
  [template]
  (into {} (mapcat (comp :events :split)) (nodes template)))
