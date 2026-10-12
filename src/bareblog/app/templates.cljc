(ns bareblog.app.templates
  "The blog templates, by the name a blog gives in its settings, and what can be read from one."
  (:require [bareblog.app.journal :as journal]
            [clojure.string :as str]))

(def default :journal)

(def by-name
  {:journal journal/template})

(defn named
  "The blog template with the name `template-name`. Throws when there is none."
  [template-name]
  (or (by-name template-name)
      (throw (ex-info "No blog template has this name." {:template template-name}))))

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

(defn- nodes
  "The node `node` of a blog template and every node below it."
  [{:keys [lists] :as node}]
  (cons node (mapcat nodes (mapcat (fn [{:keys [row rows]}] (if row [row] (vals rows))) (vals lists)))))

(defn events
  "The event meanings of every template in the blog template `template`, by event type and
   part name."
  [template]
  (into {} (mapcat (comp :events :split)) (nodes template)))
