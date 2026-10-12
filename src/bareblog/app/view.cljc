(ns bareblog.app.view
  "What a page shows, as a value.

   The view of a page is what its blog template is given: for each node its `:item`, and for
   each of its lists the keys of the rows in `:order` and what each row is given in `:by-key`."
  (:require [bareblog.address :as address]
            [bareblog.app.templates :as templates]
            [clojure.string :as str]))

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

(defn- chosen-row [chosen tag-name]
  [tag-name {:item {:name tag-name :chosen? (= chosen tag-name)}}])

(defn- all-tags
  "Every tag of `posts`, once, in the order of the alphabet."
  [posts]
  (sort (distinct (mapcat :tags posts))))

(defn- entries-node
  "The node of a page that lists the `:entries` of `listing`. The rest of `listing` says what
   the list is: its `:heading`, the page it is and of how many, whether the reader chose to see
   only some posts, and the `:tags` to choose from, of which `:chosen-tag` is chosen."
  [{:keys [entries tags chosen-tag] :as listing}]
  {:item  (merge {:heading nil :number 1 :pages 1 :filtered? false :count (count entries)}
                 (select-keys listing [:heading :number :pages :filtered?]))
   :lists {:entries  (rows (map entry-row entries))
           :all-tags (rows (map (partial chosen-row chosen-tag) tags))}})

(defn- page-count [per-page entries]
  (max 1 (quot (+ (count entries) (dec per-page)) per-page)))

(def ^:private not-found
  {:key "not-found" :named {:kind :not-found} :node {:item {:home address/root}}})

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
  [posts {:keys [query tag]}]
  (let [found (chosen posts query tag)]
    {:key   "entries"
     :named {:kind :home :number 1 :found (count found)}
     :node  (entries-node {:entries found :filtered? true :tags (all-tags posts) :chosen-tag tag})}))

(defn- paged-home
  "Page `number` of the home page, or the page that is not there when there is none."
  [{:keys [blog posts]} number]
  (let [per-page (:per-page blog default-per-page)
        pages    (page-count per-page posts)]
    (if (<= 1 number pages)
      {:key   "entries"
       :named {:kind :home :number number}
       :node  (entries-node {:entries (take per-page (drop (* per-page (dec number)) posts))
                             :number  number
                             :pages   pages
                             :tags    (all-tags posts)})}
      not-found)))

(defn- filtering?
  "True when the reader chose to see only some of the posts."
  [{:keys [query tag]}]
  (boolean (or (seq (words-of query)) tag)))

(defn- home [site {:keys [number]} _shown choice]
  (if (filtering? choice)
    (filtered-home (:posts site) choice)
    (paged-home site number)))

(defn- tag [{:keys [posts]} {tag-name :tag} _shown _choice]
  (let [tagged (filter (partial tagged? tag-name) posts)]
    (if (seq tagged)
      {:key   "entries"
       :named {:kind :tag :tag tag-name}
       :node  (entries-node {:entries tagged :heading tag-name})}
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

(defn- archive [{:keys [posts]} _page _shown _choice]
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

(defn- post [{:keys [posts]} {:keys [folder]} {:keys [title date tags blocks] :as shown} _choice]
  (if shown
    {:key   "post"
     :named {:kind :post :post title}
     :node  {:item  (merge {:title title :date date :draft? (draft? posts folder)}
                           (neighbours posts folder))
             :lists {:tags   (tag-rows tags)
                     :blocks (block-rows blocks)}}}
    not-found))

(defn- nothing [_site _page _shown _choice]
  not-found)

(def ^:private mains
  "What makes the key of each kind of page, what names it, and its node, from the site, the
   page, the post of a post page, and what the reader chose."
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

(defn page
  "What the page that `told` describes shows of the blog in `site`, for what the reader chose
   in `choice`, which has a `:query`, a `:tag`, a `:preset` and whether the menu is open: the
   name of the blog template, the language and the title of the document, and what the template
   is given."
  [{:keys [blog] :as site} {shown :post what :page} choice]
  (let [template-name            (:template blog templates/default)
        {:keys [key named node]} ((get mains (:kind what) nothing) site what shown choice)]
    {:template template-name
     :language (:language blog default-language)
     :title    (document-title ((:title (templates/named template-name)) named) (:title blog))
     :item     {:preset     (preset blog (:preset choice))
                :presets    (count (:presets blog))
                :query      (or (:query choice) "")
                :menu?      (boolean (:menu? choice))
                :blog-title (:title blog)
                :home       address/root
                :archive    address/archive}
     :lists    {:main    (rows [[key node]])
                :presets (rows (map preset-row (:presets blog)))}}))
