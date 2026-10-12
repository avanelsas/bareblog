(ns bareblog.app.view
  "What a page shows, as a value.

   The view of a page is what its blog template is given: for each node its `:item`, and for
   each of its lists the keys of the rows in `:order` and what each row is given in `:by-key`."
  (:require [bareblog.address :as address]
            [bareblog.app.templates :as templates]
            [bareblog.site :as site]
            [clojure.string :as str]))

(def between " · ")

(def default-language "en")

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
;; top of the blog is, and for each post its `:href`.

(defn- addressed
  "The `site` for a blog whose top is at `root`, with the address of each post."
  [site root]
  (-> site
      (assoc :root root)
      (update :posts (partial mapv (fn [entry] (assoc entry :href (address/post root (:folder entry))))))))

(defn- tag-row [root tag-name]
  [tag-name {:item {:name tag-name :href (address/tag root tag-name)}}])

(defn- tag-rows [root tags]
  (rows (map (partial tag-row root) (distinct tags))))

(defn- listed
  "What a post shows of itself wherever it stands in a list."
  [{:keys [title date href draft?]}]
  {:title title :date date :href href :draft? (boolean draft?)})

(defn- entry-row [root {:keys [folder summary tags] :as entry}]
  [folder {:item  (assoc (listed entry) :summary summary)
           :lists {:tags (tag-rows root tags)}}])

(defn- chosen-row [chosen tag-name]
  [tag-name {:item {:name tag-name :chosen? (= chosen tag-name)}}])

(defn- entries-node
  "The node of a page that lists the `:entries` of `listing`. The rest of `listing` says what
   the list is: its `:heading`, the page it is and of how many, whether the reader chose to see
   only some posts, and the `:tags` to choose from, of which `:chosen-tag` is chosen."
  [root {:keys [entries tags chosen-tag] :as listing}]
  {:item  (merge {:heading nil :number 1 :pages 1 :filtered? false :count (count entries)}
                 (select-keys listing [:heading :number :pages :filtered?]))
   :lists {:entries  (rows (map (partial entry-row root) entries))
           :all-tags (rows (map (partial chosen-row chosen-tag) tags))}})

(defn- not-found
  "The page for an address with no page, with the way back to the top of the blog."
  [site]
  {:key "not-found" :named {:kind :not-found} :node {:item {:home (:root site)}}})

(defn- plain-letters
  "The `text` in small letters, with every accent taken off its letter."
  [text]
  (-> #?(:clj  (java.text.Normalizer/normalize (or text "") java.text.Normalizer$Form/NFD)
         :cljs (.normalize (or text "") "NFD"))
      (str/replace #"[\u0300-\u036f]" "")
      str/lower-case))

(defn- words-of
  "The words in `text`, as a search looks for them."
  [text]
  (remove str/blank? (str/split (plain-letters text) #"\s+")))

(defn- searched
  "The text of `entry` that a search looks through, as a search looks for it."
  [{:keys [title summary tags]}]
  (plain-letters (str/join " " (concat [title summary] tags))))

;; A word is found anywhere in the text, also inside a longer word, so that a search narrows
;; while the reader is still typing.
(defn- matches?
  "True when every one of `words` is in the title, the summary or the tags of `entry`."
  [words entry]
  (let [text (searched entry)]
    (every? (partial str/includes? text) words)))

(defn- tagged? [tag-name entry]
  (some #{tag-name} (:tags entry)))

(defn- chosen
  "The posts of `posts` that the reader chose to see: those that match every word of `query`,
   and have the tag `tag-name` when there is one."
  [posts query tag-name]
  (filter (every-pred (partial matches? (words-of query))
                      (if tag-name (partial tagged? tag-name) any?))
          posts))

(defn- filtered-home
  "The home page as one list of the posts that the reader chose to see."
  [{:keys [posts] :as site} {:keys [query tag]}]
  (let [found (chosen posts query tag)]
    {:key   "entries"
     :named {:kind :home :number 1 :found (count found)}
     :node  (entries-node (:root site)
                          {:entries found :filtered? true :tags (site/tags site) :chosen-tag tag})}))

(defn- paged-home
  "Page `number` of the home page as a list of entries."
  [{:keys [posts] :as site} number]
  (let [per-page (site/per-page site)]
    {:key   "entries"
     :named {:kind :home :number number}
     :node  (entries-node (:root site)
                          {:entries (take per-page (drop (* per-page (dec number)) posts))
                           :number  number
                           :pages   (site/page-count site)
                           :tags    (site/tags site)})}))

(defn- filtering?
  "True when the reader chose to see only some of the posts."
  [{:keys [query tag]}]
  (boolean (or (seq (words-of query)) tag)))

(declare post)

;; What makes a page is given one map: the `:site`, the `:page` and the `:post` that the page
;; was told, what is `:local` to the page, and the blog `:template`.

(defn- newest-post
  "The home page as the page of the newest post, which is the post that the page was told. A
   blog with no posts has a home page with no entries."
  [{:keys [site] shown :post :as told}]
  (cond
    shown                  (assoc (post told) :named {:kind :home :number 1})
    (empty? (:posts site)) (paged-home site 1)
    :else                  (not-found site)))

(defn- entries-home
  "The home page as a list of entries: those the reader chose to see, where the home page
   shows a choice, or else the page of all of them that the address names."
  [{:keys [site page local template]}]
  (if (and (templates/chooses-on? template :home) (filtering? local))
    (filtered-home site local)
    (paged-home site (:number page))))

(def ^:private homes
  "What makes the home page, for each thing that a blog template says its home page shows."
  {:entries     entries-home
   :newest-post newest-post})

(defn- home
  "The page of the home page that the address names, or the page that is not there when the
   home page has no such page."
  [{:keys [site page template] :as told}]
  (if (<= 1 (:number page) (templates/home-pages template site))
    ((homes (:home template)) told)
    (not-found site)))

(defn- tag [{:keys [site page]}]
  (let [tag-name (:tag page)
        tagged   (filter (partial tagged? tag-name) (:posts site))]
    (if (seq tagged)
      {:key   "entries"
       :named {:kind :tag :tag tag-name}
       :node  (entries-node (:root site) {:entries tagged :heading tag-name})}
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

(defn- draft? [posts folder]
  (boolean (some (fn [entry] (and (= folder (:folder entry)) (:draft? entry))) posts)))

(defn- post [{:keys [site] {:keys [folder title date tags blocks] :as shown} :post}]
  (if shown
    {:key   "post"
     :named {:kind :post :post title}
     :node  {:item  (merge {:title title :date date :draft? (draft? (:posts site) folder)}
                           (neighbours site folder))
             :lists {:tags   (tag-rows (:root site) tags)
                     :blocks (block-rows blocks)}}}
    (not-found site)))

(defn- nothing [{:keys [site]}]
  (not-found site))

(def ^:private mains
  "What makes the key of each kind of page, what names it, and its node."
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

(defn- preset-row [preset]
  [preset {:item {:name preset}}])

(defn- preset
  "The preset of the page: the one the reader chose, when the blog offers it, or else the first
   that the blog offers."
  [{:keys [presets]} chosen]
  (or (some #{chosen} presets) (first presets)))

(defn- chosen-tag
  "The tag that the reader chose, on a page that shows a choice."
  [{:keys [page local template]}]
  (when (templates/chooses-on? template (:kind page))
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
  (rows (map preset-row (get-in site [:blog :presets]))))

(defn- tag-list
  "Every tag of the blog to choose from, with the one that the reader chose."
  [{:keys [site] :as told}]
  (rows (map (partial chosen-row (chosen-tag told)) (site/tags site))))

(defn- post-list
  "Every post, or those with the tag that the reader chose, with the post that the page shows."
  [{:keys [site] shown :post :as told}]
  (let [tag-name (chosen-tag told)
        posts    (cond->> (:posts site) tag-name (filter (partial tagged? tag-name)))]
    (rows (map (partial post-row (:folder shown) tag-name) (cons nil posts) posts))))

(def ^:private lists
  "What makes each list that the document of a blog template may ask for, by its name."
  {:presets preset-list
   :tags    tag-list
   :posts   post-list})

(defn- list-rows
  "The rows of the list `list-name` of the document. Throws when the view has no such list."
  [told list-name]
  (if-some [make (lists list-name)]
    (make told)
    (throw (ex-info "The view has no list with this name." {:list list-name}))))

(defn- command
  "The post of `entry` as a command of a palette: what is shown, what is searched, and where
   it leads."
  [{:keys [folder title summary tags href] :as entry}]
  {:id       folder
   :label    title
   :group    (or (year entry) "")
   :keywords (vec (remove nil? (cons summary tags)))
   :value    href})

(def ^:private nothing-local
  "What is local to a page that was told nothing of it, of the facts that the document is
   given as they are."
  {:query "" :menu? false :palette? false :wide? false :command-key? false})

(defn- item
  "What the document is given of the blog and of what is local to the page. A blog template
   with a palette is given the posts as its commands."
  [{:keys [site local template] :as told}]
  (let [{:keys [blog posts root]} site]
    (cond-> (merge nothing-local
                   (select-keys local (keys nothing-local))
                   {:preset     (preset blog (:preset local))
                    :presets    (count (:presets blog))
                    :tag        (chosen-tag told)
                    :blog-title (:title blog)
                    :home       root
                    :archive    (address/archive root)})
      (= :palette (:search template)) (assoc :commands (mapv command posts)))))

(defn page
  "What the page that `told` describes shows of the blog in `site`, whose top is at `root`,
   for what is local to the page in `local`: what the reader chose, a `:query`, a `:tag` and a
   `:preset`, whether the menu and the palette are open, and what the page knows of its window
   and its keyboard. The result names the blog template, the language and the title of the
   document, and holds what the template is given: an item, the page, and each other list that
   the template asks for."
  [root {:keys [blog] :as site} {shown :post what :page} local]
  (let [template                 (templates/of-blog blog)
        told                     {:site     (addressed site root)
                                  :page     what
                                  :post     shown
                                  :local    local
                                  :template template}
        {:keys [key named node]} ((get mains (:kind what) nothing) told)]
    {:template (templates/name-of blog)
     :language (:language blog default-language)
     :title    (document-title ((:title template) named) (:title blog))
     :item     (item told)
     :lists    (into {:main (rows [[key node]])}
                     (map (juxt identity (partial list-rows told)))
                     (disj (templates/list-names template) :main))}))
