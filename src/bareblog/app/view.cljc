(ns bareblog.app.view
  "What a page shows, as a value.

   The view of a page is what its blog template is given: for each node its `:item`, and for
   each of its lists the keys of the rows in `:order` and what each row is given in `:by-key`."
  (:require [bareblog.address :as address]
            [bareblog.app.templates :as templates]))

(def between " · ")

(def default-language "en")

(def default-per-page 10)

(defn- rows
  "A list of rows, from each key and what the row is given, in order."
  [keyed]
  {:order  (mapv first keyed)
   :by-key (into {} keyed)})

(defn- with-key
  "Adds `block` under a key made from its content. A block with the same content as an earlier
   one gets a count after its key."
  [{:keys [keyed seen]} block]
  (let [content (str (hash block))
        n       (inc (get seen content 0))]
    {:keyed (conj keyed [(if (= 1 n) content (str content "-" n)) block])
     :seen  (assoc seen content n)}))

(defn- block-rows [blocks]
  (rows (:keyed (reduce with-key {:keyed [] :seen {}} blocks))))

(defn- tag-row [tag-name]
  [tag-name {:item {:name tag-name :href (address/tag tag-name)}}])

(defn- tag-rows [tags]
  (rows (map tag-row (distinct tags))))

(defn- listed
  "What a post shows of itself wherever it stands in a list."
  [{:keys [folder title date draft?]}]
  {:title title :date date :href (address/post folder) :draft? (boolean draft?)})

(defn- entry-row [{:keys [folder summary tags] :as entry}]
  [folder {:item  (assoc (listed entry) :summary summary)
           :lists {:tags (tag-rows tags)}}])

(defn- entries-node
  "The node of a page that lists `entries`, under `heading` when there is one. It is page
   `number` of `pages`."
  [heading entries number pages]
  {:item  {:heading heading :count (count entries) :number number :pages pages}
   :lists {:entries (rows (map entry-row entries))}})

(defn- page-count [per-page entries]
  (max 1 (quot (+ (count entries) (dec per-page)) per-page)))

(def ^:private not-found
  {:key "not-found" :named {:kind :not-found} :node {:item {:home address/root}}})

(defn- home [{:keys [blog posts]} {:keys [number]} _shown]
  (let [per-page (:per-page blog default-per-page)
        pages    (page-count per-page posts)]
    (if (<= 1 number pages)
      {:key   "entries"
       :named {:kind :home :number number}
       :node  (entries-node nil (take per-page (drop (* per-page (dec number)) posts)) number pages)}
      not-found)))

(defn- tagged? [tag-name entry]
  (some #{tag-name} (:tags entry)))

(defn- tag [{:keys [posts]} {tag-name :tag} _shown]
  (let [tagged (filter (partial tagged? tag-name) posts)]
    (if (seq tagged)
      {:key "entries" :named {:kind :tag :tag tag-name} :node (entries-node tag-name tagged 1 1)}
      not-found)))

(defn- year [{:keys [date]}]
  (when date (subs date 0 4)))

(defn- archive-row
  "The row of `entry` in the archive. It is the first of its year when there is no entry
   `before` it, or that one has another year."
  [before entry]
  [(:folder entry) {:item (assoc (listed entry)
                                 :year           (year entry)
                                 :first-of-year? (or (nil? before) (not= (year before) (year entry))))}])

(defn- archive [{:keys [posts]} _page _shown]
  {:key   "archive"
   :named {:kind :archive}
   :node  {:item  {:count (count posts)}
           :lists {:rows (rows (map archive-row (cons nil posts) posts))}}})

(defn- neighbour
  "What a post shows of the post beside it, or nil when there is none."
  [entry]
  (when entry
    (select-keys (listed entry) [:title :href])))

(defn- neighbours
  "The entries beside the post `folder` in `posts`: the newer one and the older one."
  [posts folder]
  (let [[newer _ older] (first (filter (comp #{folder} :folder second)
                                       (partition 3 1 (concat [nil] posts [nil]))))]
    {:newer (neighbour newer) :older (neighbour older)}))

(defn- draft? [posts folder]
  (boolean (some (fn [entry] (and (= folder (:folder entry)) (:draft? entry))) posts)))

(defn- post [{:keys [posts]} {:keys [folder]} {:keys [title date tags blocks] :as shown}]
  (if shown
    {:key   "post"
     :named {:kind :post :post title}
     :node  {:item  (merge {:title title :date date :draft? (draft? posts folder)}
                           (neighbours posts folder))
             :lists {:tags   (tag-rows tags)
                     :blocks (block-rows blocks)}}}
    not-found))

(defn- nothing [_site _page _shown]
  not-found)

(def ^:private mains
  "What makes the key of each kind of page, what names it, and its node, from the site, the
   page and the post of a post page."
  {:home    home
   :tag     tag
   :archive archive
   :post    post})

(defn- document-title
  "The title of the document: the name of the page, when it has one, and the name of the blog."
  [page-name blog-name]
  (if page-name
    (str page-name between blog-name)
    blog-name))

(defn page
  "What the page that `told` describes shows of the blog in `site`: the name of the blog
   template, the language and the title of the document, and what the template is given."
  [{:keys [blog] :as site} {shown :post what :page}]
  (let [template-name            (:template blog templates/default)
        {:keys [key named node]} ((get mains (:kind what) nothing) site what shown)]
    {:template template-name
     :language (:language blog default-language)
     :title    (document-title ((:title (templates/named template-name)) named) (:title blog))
     :item     {:preset     (first (:presets blog))
                :blog-title (:title blog)
                :home       address/root
                :archive    address/archive}
     :lists    {:main (rows [[key node]])}}))
