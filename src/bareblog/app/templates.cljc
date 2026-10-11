(ns bareblog.app.templates
  "The blog templates, by the name a blog gives in its settings."
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

(defn- tag-names
  "The tag of every element in the fixed template `fixed`."
  [fixed]
  (map (comp name first) (filter vector? (tree-seq vector? seq fixed))))

(defn- custom? [tag-name]
  (str/includes? tag-name "-"))

(defn- fixed-templates
  "Every fixed template that the blog template `template` puts in a page for `blocks`."
  [{:keys [page tag block]} blocks]
  (concat [(:fixed page) (:fixed tag)] (map block blocks)))

(defn custom-elements
  "The tags of the custom elements that the blog template `template` puts in a page for
   `blocks`."
  [template blocks]
  (into (sorted-set) (comp (mapcat tag-names) (filter custom?)) (fixed-templates template blocks)))
