(ns bareblog.blog.shape
  "The shape of a blog: what its blog template says of it, apart from how it looks.

   A shape says three things. `:home` is what the home page shows, `:entries` or
   `:newest-post`. `:choice` is where what a reader chose takes effect, `:home` or
   `:everywhere`. `:search` is how a reader searches, with a `:field` or a `:palette`.

   The command reads a shape to know which pages a blog has, and the app to know what a page
   shows. Neither names a blog template. How a blog template looks is in `bareblog.app`, which
   has a node for each name here."
  (:require [bareblog.blog.site :as site]))

(def default-template
  "The name of the blog template of a blog that names none."
  :journal)

(def by-name
  "The shape of each blog template, by the name a blog gives in its settings."
  {:journal  {:home :entries :choice :home :search :field}
   :notebook {:home :newest-post :choice :everywhere :search :palette}
   :magazine {:home :entries :choice :home :search :field}
   :longform {:home :entries :choice :home :search :field}
   :diary    {:home :entries :choice :home :search :field}})

(defn named
  "The shape of the blog template with the name `template-name`. Throws when there is none."
  [template-name]
  (or (by-name template-name)
      (throw (ex-info "No blog template has this name." {:template template-name}))))

(defn template-name
  "The name of the blog template that the settings `blog` name, or of the default one when
   they name none."
  [blog]
  (:template blog default-template))

(defn of-blog
  "The shape of the blog with the settings `blog`."
  [blog]
  (named (template-name blog)))

(def ^:private shows-choice
  "The kinds of page that show what a reader chose, for each place that a shape names."
  {:home       #{:home}
   :everywhere some?})

(defn chooses-on?
  "True when a page of the kind `kind` shows what a reader chose of the posts, in a blog of
   the shape `shape`."
  [shape kind]
  (boolean ((shows-choice (:choice shape)) kind)))

(defn home-pages
  "How many pages the home page of the blog in `site` has, in a blog of the shape `shape`: as
   many as its entries fill, or one where it shows a post."
  [shape site]
  (if (= :entries (:home shape))
    (site/page-count site)
    1))
