(ns bareblog.app.view
  "What a page shows, as a value.

   The view of a page is what its blog template is given: for each node its `:item`, and for
   each of its lists the keys of the rows in `:order` and what each row is given in `:by-key`."
  (:require [bareblog.app.card :as card]
            [bareblog.app.templates :as templates]
            [bareblog.blog.address :as address]
            [bareblog.blog.comments :as comments]
            [bareblog.blog.series :as series]
            [bareblog.blog.shape :as shape]
            [bareblog.blog.site :as site]
            [bareblog.blog.text :as text]
            [clojure.string :as str]))

(def between " · ")

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

;; Inside the view the site knows two things more than it was read with: `:root`, where the
;; top of the blog is, and for each post its `:href`, with the address of its picture.

(defn- addressed-entry
  "The `entry` of a post or a page, in a blog whose top is at `root`, with its address, and
   its picture at the address of its file."
  [root {:keys [folder picture] :as entry}]
  (let [href (address/post root folder)]
    (cond-> (assoc entry :href href)
      picture (assoc :picture (text/place href picture)))))

(defn- addressed
  "The `site` for a blog whose top is at `root`, with the address of each post and of each
   page that is no post."
  [site root]
  (let [addressed-entries (partial mapv (partial addressed-entry root))]
    (cond-> (assoc site :root root)
      (:posts site) (update :posts addressed-entries)
      (:pages site) (update :pages addressed-entries))))

(defn- picture-address
  "The address of `picture`, a picture of the blog at `root`, with its version, or nil."
  [root {:keys [src version]}]
  (when src
    (str (address/file root (address/encoded src)) "?v=" version)))

(defn- picture-addresses
  "The address of each picture of the kind `kind` in `site`, by mode."
  [{:keys [pictures root]} kind]
  (update-vals (get pictures kind) (partial picture-address root)))

(defn- tag-row [root tag-name]
  [tag-name {:item {:name tag-name :href (address/tag root tag-name)}}])

(defn- tag-rows [root tags]
  (rows (map (partial tag-row root) (distinct tags))))

(defn- listed
  "What a post shows of itself wherever it stands in a list."
  [{:keys [title date href draft?]}]
  {:title title :date (site/day date) :href href :draft? (boolean draft?)})

(defn- entry-row [{:keys [root blog]} front? first-picture? {:keys [folder summary picture tags] :as entry}]
  [folder {:item  (assoc (listed entry)
                         :folder           folder
                         :summary          summary
                         :picture          picture
                         :front?           front?
                         :first-picture?   first-picture?
                         :gradient-titles? (site/gradient-titles? blog))
           :lists {:tags (tag-rows root tags)}}])

(defn- first-pictures
  "Whether each of `entries` is the first of them that has a picture."
  [entries]
  (let [first-picture (:folder (first (filter :picture entries)))]
    (map (comp (partial = first-picture) :folder) entries)))

(defn- chosen-row [chosen tag-name]
  [tag-name {:item {:name tag-name :chosen? (= chosen tag-name)}}])

(defn- entries-node
  "The node of a page that lists the `:entries` of `listing`. The rest of `listing` says what
   the list is: its `:heading`, the page it is and of how many, whether the reader chose to see
   only some posts, and the `:tags` to choose from, of which `:chosen-tag` is chosen.
   `:front?` says that it is the first page of the home page, with nothing chosen, and `:date`
   is the date that the reader chose. The list also knows the days of the posts of `site`, and
   the language of the blog."
  [{:keys [blog] :as site} {:keys [entries tags chosen-tag front?] :as listing}]
  {:item  (merge {:heading nil :number 1 :pages 1 :filtered? false :count (count entries)
                  :date nil :days (site/post-days site) :language (site/language blog)}
                 (select-keys listing [:heading :number :pages :filtered? :date]))
   :lists {:entries  (rows (map (partial entry-row site (boolean front?))
                                (first-pictures entries)
                                entries))
           :all-tags (rows (map (partial chosen-row chosen-tag) tags))}})

(defn- not-found
  "The page for an address with no page, with the way back to the top of the blog."
  [site]
  {:key "not-found" :named {:kind :not-found} :node {:item {:home (:root site)}}})

(defn- words-of
  "The words in `text`, as a search looks for them."
  [text]
  (remove str/blank? (str/split (text/plain-letters text) #"\s+")))

(defn- searched
  "The text of `entry` that a search looks through, as a search looks for it."
  [{:keys [title summary tags]}]
  (text/plain-letters (str/join " " (concat [title summary] tags))))

;; A word is found anywhere in the text, also inside a longer word, so that a search narrows
;; while the reader is still typing.
(defn- matches?
  "True when every one of `words` is in the title, the summary or the tags of `entry`."
  [words entry]
  (let [text (searched entry)]
    (every? (partial str/includes? text) words)))

(defn- tagged? [tag-name entry]
  (some #{tag-name} (:tags entry)))

(defn- dated?
  "True when `entry` is of the date `date`: a day, as `2026-10-12`, or a month, as `2026-10`."
  [date entry]
  (str/starts-with? (str (:date entry)) date))

(defn- chosen
  "The posts of `posts` that the reader chose to see: those that match every word of `query`,
   have the tag `tag-name` when there is one, and are of the date `date` when there is one."
  [posts {:keys [query tag date]}]
  (filter (every-pred (partial matches? (words-of query))
                      (if tag (partial tagged? tag) any?)
                      (if date (partial dated? date) any?))
          posts))

(defn- filtered-home
  "The home page as one list of the posts that the reader chose to see."
  [{:keys [posts] :as site} {:keys [tag date] :as local}]
  (let [found (chosen posts local)]
    {:key   "entries"
     :named {:kind :home :number 1 :found (count found)}
     :node  (entries-node site
                          {:entries    found
                           :filtered?  true
                           :tags       (site/tags site)
                           :chosen-tag tag
                           :date       date})}))

(defn- paged-home
  "Page `number` of the home page as a list of entries."
  [{:keys [posts] :as site} number]
  (let [per-page (site/per-page site)]
    {:key   "entries"
     :named {:kind :home :number number}
     :node  (entries-node site
                          {:entries (take per-page (drop (* per-page (dec number)) posts))
                           :number  number
                           :front?  (= 1 number)
                           :pages   (site/page-count site)
                           :tags    (site/tags site)})}))

(defn- filtering?
  "True when the reader chose to see only some of the posts."
  [{:keys [query tag date]}]
  (boolean (or (seq (words-of query)) tag date)))

(declare post)

;; What makes a page is given one map: the `:site`, the `:page` and the `:text` that the page
;; was told, what is `:local` to the page, and the blog `:template`.

(defn- newest-post
  "The home page as the page of the newest post, which is the post that the page was told. A
   blog with no posts has a home page with no entries."
  [{:keys [site] shown :text :as told}]
  (cond
    shown                  (assoc (post told) :named {:kind :home :number 1})
    (empty? (:posts site)) (paged-home site 1)
    :else                  (not-found site)))

(defn- entries-home
  "The home page as a list of entries: those the reader chose to see, where the home page
   shows a choice, or else the page of all of them that the address names."
  [{:keys [site page local shape]}]
  (if (and (shape/chooses-on? shape :home) (filtering? local))
    (filtered-home site local)
    (paged-home site (:number page))))

(def ^:private homes
  "What makes the home page, for each thing that a blog template says its home page shows."
  {:entries     entries-home
   :newest-post newest-post})

(defn- home
  "The page of the home page that the address names, or the page that is not there when the
   home page has no such page."
  [{:keys [site page shape] :as told}]
  (if (<= 1 (:number page) (shape/home-pages shape site))
    ((homes (:home shape)) told)
    (not-found site)))

(defn- tag [{:keys [site page]}]
  (let [tag-name (:tag page)
        tagged   (filter (partial tagged? tag-name) (:posts site))]
    (if (seq tagged)
      {:key   "entries"
       :named {:kind :tag :tag tag-name}
       :node  (entries-node site {:entries tagged :heading tag-name})}
      (not-found site))))

(defn- year [{:keys [date]}]
  (when date (subs date 0 4)))

(defn- by-year
  "What `entry` shows of its year in a list by date. It is the first of its year when there is
   no entry `before` it, or that one has another year."
  [before entry]
  {:year           (year entry)
   :first-of-year? (or (nil? before) (not= (year before) (year entry)))})

(defn- archive-row [before entry]
  [(:folder entry) {:item (merge (listed entry) (by-year before entry))}])

(defn- archive [{:keys [site]}]
  (let [posts (:posts site)]
    {:key   "archive"
     :named {:kind :archive}
     :node  {:item  {:count (count posts)}
             :lists {:rows (rows (map archive-row (cons nil posts) posts))}}}))

(defn- neighbour
  "What a post shows of the post beside it, or nil when there is none."
  [entry]
  (when entry
    (select-keys (listed entry) [:title :href])))

(defn- neighbours
  "The entries beside the post `folder` in `site`: the newer one and the older one."
  [{:keys [posts]} folder]
  (let [[newer _ older] (first (filter (comp #{folder} :folder second)
                                       (partition 3 1 (concat [nil] posts [nil]))))]
    {:newer (neighbour newer) :older (neighbour older)}))

(defn- marked?
  "True when the entry of `folder` among `entries` has `mark`."
  [entries folder mark]
  (boolean (some (fn [entry] (and (= folder (:folder entry)) (mark entry))) entries)))

(defn- part-row
  "The part of a series that stands `i`th from 0, in a series of which the post in `folder` is
   the `number`th from 1."
  [number folder i {:keys [title href] part :folder}]
  [part {:item {:title    title
                :href     href
                :number   (inc i)
                :current? (= folder part)
                :read?    (< (inc i) number)}}])

(defn- series-item
  "What the post shows of `in-series`, the series it is a part of: its name, where the post
   stands in it, and the next part."
  [{series-name :name :keys [parts number next]}]
  {:name   series-name
   :number number
   :count  (count parts)
   :next   (some-> next (select-keys [:title :href]))})

(defn- part-rows
  "The parts of `in-series`, the series that the post in `folder` is a part of, or none."
  [folder {:keys [parts number] :as in-series}]
  (rows (when in-series (map-indexed (partial part-row number folder) parts))))

(defn- commenter
  "The name that `comment` shows, where `author` is the name of the author of the blog."
  [author {:keys [author?] commenter-name :name}]
  (if author? author commenter-name))

(defn- comment-row
  "The row of `comment`, with the name of the comment it answers among `by-id`. A comment is a
   fixed template, as a block is, so its row is what the template is made from."
  [author by-id {:keys [id reply-to moment depth author? short-id blocks] :as comment}]
  [id (cond-> {:id            id
               :name          (commenter author comment)
               :day           (site/day moment)
               :depth         depth
               :reply-to-name (some->> (by-id reply-to) (commenter author))
               :author?       (boolean author?)
               :blocks        blocks}
        short-id (assoc :short-id short-id))])

;; A comment of the author holds no name, so that a new name in the settings reaches every one.
(defn- comment-rows
  "The comments of a post, as a thread shows them, in the blog with the settings `blog`."
  [blog shown-comments]
  (let [author (or (:author blog) (:title blog))
        by-id  (into {} (map (juxt :id identity)) shown-comments)]
    (rows (map (partial comment-row author by-id) (comments/thread shown-comments)))))

(defn- post [{:keys [site local] {:keys [folder title date tags] :as shown} :text}]
  (if shown
    (let [{:keys [cover blocks]} (text/split-cover shown)
          in-series              (series/of-post (:series site) (:posts site) folder)]
      {:key   "post"
       :named {:kind :post :name title}
       :node  {:item  (merge {:folder           folder
                              :title            title
                              :date             (site/day date)
                              :draft?           (marked? (:posts site) folder :draft?)
                              :cover            cover
                              :series           (some-> in-series series-item)
                              :read             (:read local)
                              :gradient-titles? (site/gradient-titles? (:blog site))
                              :comments?        (boolean (seq (:comments shown)))}
                             (neighbours site folder))
               :lists {:tags     (tag-rows (:root site) tags)
                       :blocks   (block-rows blocks)
                       :parts    (part-rows folder in-series)
                       :comments (comment-rows (:blog site) (:comments shown))}}})
    (not-found site)))

;; A page of the author that is no post is shown as its text alone: it has no date, no tags
;; and no post beside it.
(defn- standalone [{:keys [site] {:keys [folder title blocks] :as shown} :text}]
  (if shown
    {:key   "standalone"
     :named {:kind :standalone :name title}
     :node  {:item  {:title            title
                     :unpublished?     (marked? (:pages site) folder :unpublished?)
                     :gradient-titles? (site/gradient-titles? (:blog site))}
             :lists {:blocks (block-rows blocks)}}}
    (not-found site)))

(defn- nothing [{:keys [site]}]
  (not-found site))

(def ^:private mains
  "What makes the key of each kind of page, what names it, and its node."
  {:home       home
   :tag        tag
   :archive    archive
   :post       post
   :standalone standalone})

(defn- document-title
  "The title of the document: the name of the page, when it has one, and the name of the blog."
  [page-name blog-name]
  (if page-name
    (str page-name between blog-name)
    blog-name))

(defn- preset-row [preset]
  [preset {:item {:name preset}}])

(defn- preset
  "The preset of the page: the one the reader chose, when the blog offers it, or else the first
   that the blog offers."
  [{themes :available-themes} chosen]
  (or (some #{chosen} themes) (first themes)))

(defn- chosen-tag
  "The tag that the reader chose, on a page that shows a choice."
  [{:keys [page local shape]}]
  (when (shape/chooses-on? shape (:kind page))
    (:tag local)))

(defn- post-row
  "The row of `entry` in a list of every post, on the page of the post `current`. Its link
   carries the tag `tag-name` when the reader chose one."
  [current tag-name before entry]
  [(:folder entry)
   {:item (merge (listed entry)
                 (by-year before entry)
                 {:href     (str (:href entry) (address/query {:tag (or tag-name "")}))
                  :current? (= current (:folder entry))})}])

(defn- preset-list [{:keys [site]}]
  (rows (map preset-row (get-in site [:blog :available-themes]))))

(defn- tag-list
  "Every tag of the blog to choose from, with the one that the reader chose."
  [{:keys [site] :as told}]
  (rows (map (partial chosen-row (chosen-tag told)) (site/tags site))))

(defn- post-list
  "Every post, or those with the tag that the reader chose, with the post that the page shows."
  [{:keys [site] shown :text :as told}]
  (let [tag-name (chosen-tag told)
        posts    (cond->> (:posts site) tag-name (filter (partial tagged? tag-name)))]
    (rows (map (partial post-row (:folder shown) tag-name) (cons nil posts) posts))))

(defn- page-link
  "The link to the page of `entry`, on the page `page`."
  [page {:keys [folder title href]}]
  [folder {:item {:title    title
                  :href     href
                  :current? (and (= :standalone (:kind page)) (= folder (:folder page)))}}])

(defn- page-list
  "The pages of the author that are no posts, in the order of their links."
  [{:keys [site page]}]
  (rows (map (partial page-link page) (:pages site))))

(def ^:private lists
  "What makes each list that the document of a blog template may ask for, by its name."
  {:presets preset-list
   :tags    tag-list
   :posts   post-list
   :pages   page-list})

(defn- list-rows
  "The rows of the list `list-name` of the document. Throws when the view has no such list."
  [told list-name]
  (if-some [make (lists list-name)]
    (make told)
    (throw (ex-info "The view has no list with this name." {:list list-name}))))

;; A command says what it is of, a year or a page. The words for that are a blog template's.
(defn- command
  "The post or the page of `entry` as a command of a palette: what is shown, what is searched,
   where it leads, and the year of a post."
  [{:keys [folder title summary tags href] :as entry}]
  (cond-> {:id       folder
           :label    title
           :keywords (vec (remove nil? (cons summary tags)))
           :value    href}
    (year entry) (assoc :year (year entry))))

(defn- commands
  "Every place of the blog in `site` that a reader can go to by name: its posts, and then its
   pages that are no posts."
  [{:keys [posts pages]}]
  (into (mapv command posts)
        (map (fn [entry] (assoc (command entry) :page? true)))
        pages))

(def ^:private nothing-local
  "What is local to a page that was told nothing of it, of the facts that the document is
   given as they are."
  {:query "" :menu? false :palette? false :command-key? false})

(defn- item
  "What the document is given of the blog and of what is local to the page. A blog template
   with a palette is given the posts as its commands."
  [{:keys [site local shape] :as told}]
  (let [{:keys [blog root]} site]
    (cond-> (merge nothing-local
                   (select-keys local (keys nothing-local))
                   {:preset             (preset blog (:preset local))
                    :logo               (picture-addresses site :logo)
                    :presets            (count (:available-themes blog))
                    :tag                (chosen-tag told)
                    :blog-title         (:title blog)
                    :powered-by-footer? (site/powered-by-footer? blog)
                    :navbar-colors?     (site/navbar-colors? blog)
                    :page-colors?       (site/page-colors? blog)
                    :texture            (name (site/texture blog))
                    :home               root
                    :archive            (address/archive root)
                    :feed               (address/feed root)})
      (= :palette (:search shape)) (assoc :commands (commands site)))))

(defn page
  "What the page that `told` describes shows of the blog in `site`, whose top is at `root`,
   for what is local to the page in `local`: what the reader chose, a `:query`, a `:tag` and a
   `:preset`, whether the menu and the palette are open, and what the page knows of its
   keyboard. The result names the blog template, the language, the title, the card and the
   favicon of the document, and holds what the template is given: an item, the page, and each
   other list that the template asks for."
  [root {:keys [blog] :as site} {shown :text what :page} local]
  (let [template                 (templates/of-blog blog)
        told                     {:site     (addressed site root)
                                  :page     what
                                  :text     shown
                                  :local    local
                                  :shape    (shape/of-blog blog)}
        {:keys [key named node]} ((get mains (:kind what) nothing) told)]
    {:template (shape/template-name blog)
     :language (site/language blog)
     :title    (document-title ((:title template) named) (:title blog))
     :card     (when-not (= :not-found (:kind named)) (card/card told))
     :favicon  (picture-addresses (:site told) :favicon)
     :item     (item told)
     :lists    (into {:main (rows [[key node]])}
                     (map (juxt identity (partial list-rows told)))
                     (disj (templates/list-names template) :main))}))
