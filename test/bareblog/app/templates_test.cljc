(ns bareblog.app.templates-test
  (:require [bareblog.app.blocks-test :refer [all-fixed? examples]]
            [bareblog.app.pages :as pages]
            [bareblog.app.templates :as templates]
            [bareblog.app.view :as view]
            [bareblog.blog.shape :as shape]
            [bareblog.blog.wire :as wire]
            [baremirror.alpha.template :as template]
            [clojure.test :refer [deftest is testing]]))

(def site
  {:blog  {:title "Bare notes" :template :journal :available-themes ["ocean"]}
   :posts [{:folder "b" :title "Second" :date "2026-02-01" :tags ["travel"] :summary "Two." :draft? true}
           {:folder "a" :title "First" :date "2025-01-01" :summary "One." :picture "a.png"}]
   :pages [{:folder "about" :title "About" :order 1 :unpublished? true}
           {:folder "contact" :title "Contact"}]})

(defn- site-of
  "The site as a blog with the blog template `template-name`."
  [template-name]
  (assoc-in site [:blog :template] template-name))

(def pages
  "A page of each kind, as a page is told it."
  {:home      {:page {:kind :home :number 1}}
   :newest    {:page {:kind :home :number 1} :text {:title "Second" :folder "b" :blocks []}}
   :tag       {:page {:kind :tag :tag "travel"}}
   :archive   {:page {:kind :archive}}
   :post      {:page {:kind :post :folder "a"}
               :text {:folder "a" :title "First" :tags ["travel"] :blocks (vec (vals examples))}}
   :standalone {:page {:kind :standalone :folder "about"}
                :text {:folder "about" :title "About" :blocks (vec (vals examples))}}
   :not-found {:page {:kind :not-found}}})

(defn- part-names
  "The part names in the fixed template `fixed`."
  [fixed]
  (set (keep :data-x-part (filter map? (tree-seq coll? seq fixed)))))

(deftest named-test
  (testing "there is a blog template for every shape, and a shape for every blog template"
    (is (= (set (keys shape/by-name)) (set (keys templates/by-name)))))
  (testing "a name with no blog template is refused, with the name"
    (is (= {:template :poster}
           (ex-data (try (templates/named :poster) (catch #?(:clj Exception :cljs :default) e e)))))))

(deftest every-blog-template-test
  (doseq [[template-name blog-template] templates/by-name]
    (testing (str template-name " has a key that is text")
      (is (string? (:key blog-template))))
    (doseq [{:keys [split fixed-of lists]} (templates/nodes blog-template)]
      (testing (str template-name ": a node has a split template or a function that gives a fixed one")
        (is (or (template/split? split) (fn? fixed-of))))
      (testing (str template-name ": a split template is fixed all the way down once it is split")
        (is (or (nil? split) (all-fixed? (:fixed split)))))
      (testing (str template-name ": each list names a part that its node has")
        (is (every? (part-names (:fixed split)) (map :part (vals lists))))))
    (testing (str template-name " shows every kind of page with templates that are fixed all the way down")
      (doseq [[kind told] pages]
        (is (every? all-fixed? (templates/fixed-templates blog-template
                                                          (view/page "/" (site-of template-name) told {})))
            kind)))))

(deftest keys-test
  (testing "every blog template has its own key, so that a page is never taken for another's"
    (is (apply distinct? (map :key (vals templates/by-name))))))

(deftest row-node-test
  (let [{:keys [lists]} (templates/named :journal)]
    (testing "a list of the document has a node for the page of each key"
      (is (some? (templates/row-node (:main lists) "post")))
      (is (some? (templates/row-node (:main lists) "not-found"))))
    (testing "a key with no node is refused, with the key"
      (is (= {:key "poster"}
             (ex-data (try (templates/row-node (:main lists) "poster")
                           (catch #?(:clj Exception :cljs :default) e e))))))))

(defn- shown-nodes
  "Each node that `node` puts in a page for `shown`, with what the view gives it."
  [node shown]
  (cons [node shown]
        (for [[list-name row-list] (:lists node)
              :let                 [{:keys [order by-key]} (templates/rows-of row-list list-name shown)]
              k                    order
              pair                 (shown-nodes (templates/row-node row-list k) (by-key k))]
          pair)))

(defn- keyword-holes
  "The holes of `split` that are a key of the item."
  [split]
  (filter keyword? (mapcat (fn [{:keys [attrs text props]}] (concat [text] (vals attrs) (vals props)))
                           (:holes split))))

(deftest view-and-template-test
  (doseq [[template-name blog-template] templates/by-name
          :let                          [site (site-of template-name)]]
    (testing (str "the view of every kind of page names a page that " template-name " has a node for")
      (doseq [[kind told] pages]
        (is (some? (templates/row-node (get-in blog-template [:lists :main])
                                       (first (get-in (view/page "/" site told {}) [:lists :main :order]))))
            kind)))
    (testing (str template-name ": the view gives every node the key of each hole that is a key")
      (doseq [[kind told]             pages
              [{:keys [split]} shown] (shown-nodes blog-template (view/page "/" site told {}))
              hole                    (keyword-holes split)]
        (is (contains? (:item shown) hole) (str kind " " hole))))
    (testing (str template-name ": every hole of every node can be filled from what the view gives")
      (doseq [[kind told]             pages
              [{:keys [split]} shown] (shown-nodes blog-template (view/page "/" site told {}))
              :when                   split]
        (is (map? (template/writes split (:item shown))) (str kind))))))

(deftest words-test
  (testing "Journal names each kind of page in the title of the document"
    (let [title (:title (templates/named :journal))]
      (is (nil? (title {:kind :home :number 1})))
      (is (= "Page 2" (title {:kind :home :number 2})))
      (is (= "travel" (title {:kind :tag :tag "travel"})))
      (is (= "Archive" (title {:kind :archive})))
      (is (= "A week in Lisbon" (title {:kind :post :name "A week in Lisbon"})))
      (is (= "Not found" (title {:kind :not-found}))))))

(defn- elements
  "Every element of the fixed template `fixed`, as its tag, its attributes and its children."
  [fixed]
  (map template/pieces (filter vector? (tree-seq vector? seq fixed))))

(defn- named-part
  "What to call the element of `fixed` that is the part `part-name`: its tag and its class."
  [fixed part-name]
  (some (fn [[tag attrs]]
          (when (= part-name (:data-x-part attrs))
            (str (name tag) (some->> (:class attrs) (str ".")))))
        (elements fixed)))

(defn- hidden
  "The parts of the node of the page that hide themselves, when the page `told` shows `site`
   for what is `local` to the page, each as its tag and its class."
  ([site told]
   (hidden site told {}))
  ([site told local]
   (let [journal         (templates/named :journal)
         page            (view/page "/" site told local)
         k               (first (get-in page [:lists :main :order]))
         {:keys [split]} (templates/row-node (get-in journal [:lists :main]) k)
         shown           (get-in page [:lists :main :by-key k])]
     (into #{}
           (keep (fn [[part-name {:keys [attrs]}]]
                   (when (true? (:hidden attrs))
                     (named-part (:fixed split) part-name))))
           (template/writes split (:item shown))))))

(deftest hidden-test
  (let [site        (assoc site :series {"Trip" ["c" "b" "a" "0"]})
        reading     {:read 0.4}
        empty-site  (assoc site :posts [])
        two-pages   (assoc-in site [:blog :per-page] 1)
        older       {:folder "0" :title "Zero" :date "2024-01-01"}
        newer       {:folder "c" :title "Third" :date "2027-01-01"}
        post-of     (fn [folder title blocks]
                      {:page {:kind :post :folder folder} :text {:folder folder :title title :blocks blocks}})
        second-post (post-of "b" "Second" [{:kind :photo :src "b.jpg"}])
        first-post  (post-of "a" "First" [{:kind :photo :src "a.jpg"}])]
    (testing "a home page on one page hides its heading, the way back to all posts, the text for
              no posts and its page numbers"
      (is (= #{"div.heading" "button.clear" "p.empty" "x-pagination"} (hidden site (:home pages)))))
    (testing "a home page of more pages shows its page numbers"
      (is (= #{"div.heading" "button.clear" "p.empty"} (hidden two-pages (:home pages)))))
    (testing "the page of a tag shows its heading"
      (is (= #{"button.clear" "p.empty" "x-pagination"} (hidden site (:tag pages)))))
    (testing "a home page with no posts shows the text for that"
      (is (= #{"div.heading" "button.clear" "x-pagination"} (hidden empty-site (:home pages)))))
    (testing "a search shows how many posts it found and the way back to all posts"
      (is (= #{"p.empty" "x-pagination"} (hidden site (:home pages) {:query "first"}))))
    (testing "a search that finds nothing does not say that the blog has no posts"
      (is (= #{"p.empty" "x-pagination"} (hidden site (:home pages) {:query "zebra"}))))
    (testing "the archive hides the text for no posts, and shows it when there are none"
      (is (= #{"p.empty"} (hidden site (:archive pages))))
      (is (= #{} (hidden empty-site (:archive pages)))))
    (testing "the page of a draft with a newer and an older post hides nothing"
      (is (= #{} (hidden (update site :posts (partial into [newer])) second-post reading))))
    (testing "the page of the newest post, a draft, hides the link to a newer one"
      (is (= #{"a.newer"} (hidden site second-post reading))))
    (testing "the page of a published post with both neighbours hides the badge of a draft"
      (is (= #{"x-badge.draft"} (hidden (update site :posts conj older) first-post reading))))
    (testing "the page of the oldest post hides the link to an older one too, and as the last part
              of its series the link to the next part"
      (is (= #{"x-badge.draft" "a.older" "a.series-next"} (hidden site first-post reading))))
    (testing "the page of a post in no series hides the series"
      (is (= #{"a.newer" "x-card.series-group" "a.series-next"}
             (hidden (dissoc site :series) second-post reading))))
    (testing "the page of a post that fits in the window hides the line of how far it is read"
      (is (= #{"a.newer" "x-progress.read-progress"} (hidden site second-post))))
    (testing "the page of a post with no cover hides the place of its cover"
      (is (= #{"a.newer" "figure.post-cover"} (hidden site (post-of "b" "Second" []) reading))))))

(deftest contents-test
  (doseq [template-name (keys templates/by-name)
          :let          [node   (templates/row-node (get-in (templates/named template-name) [:lists :main]) "post")
                         els    (elements (:fixed (:split node)))
                         toc    (first (for [[tag attrs] els :when (= :x-toc tag)] attrs))
                         target (for [[_ attrs] els :when (= (:for toc) (:id attrs))] attrs)]]
    (testing (str template-name ": the contents of a post read its blocks part, under their label")
      (is (some? toc))
      (is (= ["blocks"] (map :data-x-part target)))
      (is (= (:content pages/words) (:label toc))))))

(deftest menu-test
  (let [journal (templates/named :journal)
        written (fn [choice] (template/writes (:split journal) (:item (view/page "/" site (:home pages) choice))))
        drawer  (fn [choice] (some (fn [[part-name write]]
                                     (when (= "x-drawer" (named-part (:fixed (:split journal)) part-name))
                                       (:attrs write)))
                                   (written choice)))]
    (testing "the menu is closed until the reader opens it"
      (is (false? (:open (drawer {}))))
      (is (true? (:open (drawer {:menu? true})))))
    (testing "the search field is there twice, in the navbar and in the menu, with the same text"
      (is (= [{:value "lisbon"} {:value "lisbon"}]
             (keep (fn [[part-name write]]
                     (when (= "x-search-field" (named-part (:fixed (:split journal)) part-name))
                       (:attrs write)))
                   (written {:query "lisbon"})))))
    (testing "both theme selects list the presets of the blog"
      (is (= [:presets :presets]
             (mapv (fn [[list-name row-list]] (:of row-list list-name))
                   (select-keys (:lists journal) [:presets :menu-presets])))))))

(defn- attrs-of
  "The attributes that `split` writes for `item` on its element with the tag and class `named`."
  [split item named]
  (some (fn [[part-name write]]
          (when (= named (named-part (:fixed split) part-name))
            (:attrs write)))
        (template/writes split item)))

(defn- text-of
  "The text that `split` writes for `item` in its first element with the tag and class `named`."
  [split item named]
  (some (fn [[part-name write]]
          (when (= named (named-part (:fixed split) part-name))
            (:text write)))
        (template/writes split item)))

(defn- entry-items
  "The item of each entry on the page `told` of a Magazine blog, in order."
  [told]
  (let [[_ node] (first (:by-key (get-in (view/page "/" (site-of :magazine) told {}) [:lists :main])))
        {:keys [order by-key]} (get-in node [:lists :entries])]
    (map (comp :item by-key) order)))

(deftest gradient-titles-test
  (doseq [[template-name row-name] [[:journal "post"] [:journal "standalone"] [:journal "entries"]
                                    [:magazine "entries"] [:longform "entries"] [:longform "post"]
                                    [:essay "entries"] [:essay "post"]]
          :let [node (templates/row-node (get-in (templates/named template-name) [:lists :main]) row-name)
                row  (or (get-in node [:lists :entries :row :split]) (:split node))]]
    (testing (str template-name " " row-name ": a title is filled with a gradient when the view says so")
      (is (true? (:gradient (attrs-of row {:gradient-titles? true} "x-typography.title"))))
      (is (false? (:gradient (attrs-of row {:gradient-titles? false} "x-typography.title"))))))
  (testing "longform: a title over the cover of a post is not filled with a gradient"
    (let [post (:split (templates/row-node (get-in (templates/named :longform) [:lists :main]) "post"))]
      (is (false? (:gradient (attrs-of post {:gradient-titles? true :cover {:src "a.png"}} "x-typography.title")))))))

(deftest navbar-colors-test
  (doseq [template-name (keys templates/by-name)
          :let          [split (:split (templates/named template-name))
                         item  (:item (view/page "/" (site-of template-name) (:home pages) {}))]]
    (testing (str template-name ": the band of the navbar says it shows the colours when the view says so")
      (is (true? (:data-colors (attrs-of split (assoc item :navbar-colors? true) "x-gaussian-blur.navbar-band"))))
      (is (false? (:data-colors (attrs-of split (assoc item :navbar-colors? false) "x-gaussian-blur.navbar-band")))))))

(deftest page-colors-test
  (doseq [template-name (keys templates/by-name)
          :let          [split (:split (templates/named template-name))
                         item  (:item (view/page "/" (site-of template-name) (:home pages) {}))]]
    (testing (str template-name ": the colours behind the page say they show when the view says so")
      (is (true? (:data-colors (attrs-of split (assoc item :page-colors? true) "x-gaussian-blur.page-colors"))))
      (is (false? (:data-colors (attrs-of split (assoc item :page-colors? false) "x-gaussian-blur.page-colors")))))))

(deftest magazine-test
  (let [entries (get-in (templates/named :magazine) [:lists :main :rows "entries"])
        card    (get-in entries [:lists :entries :row :split])
        spans   (fn [told]
                  (map (fn [item] (select-keys (attrs-of card item "x-bento-item") [:col-span :row-span :order]))
                       (entry-items told)))]
    (testing "a list of posts is a grid of cards, which a reader is told is a list"
      (is (= "x-bento-grid.cards" (named-part (:fixed (:split entries)) "entries")))
      (is (= "list" (some (fn [[tag attrs]] (when (= :x-bento-grid tag) (:role attrs)))
                          (elements (:fixed (:split entries)))))))
    (testing "the newest post with a picture on the first page leads: shown first, two columns wide
              and two rows high"
      (is (= [{:col-span 1 :row-span 1 :order nil} {:col-span 2 :row-span 2 :order -1}]
             (spans (:home pages)))))
    (testing "on the page of a tag no card leads"
      (is (every? #{{:col-span 1 :row-span 1 :order nil}} (spans (:tag pages)))))))

(deftest longform-test
  (let [longform (templates/named :longform)
        deck     (get-in longform [:lists :main :rows "entries"])
        post     (:split (get-in longform [:lists :main :rows "post"]))
        hero     (fn [cover] (attrs-of post {:title "A post" :cover cover} "x-scroll-parallax"))
        tram     {:src "/a/tram.jpg" :alt "A tram"}]
    (testing "a list of posts is a deck of cards, which a reader is told is a list"
      (is (= "x-scroll-stack.deck" (named-part (:fixed (:split deck)) "entries")))
      (is (= "list" (some (fn [[tag attrs]] (when (= :x-scroll-stack tag) (:role attrs)))
                          (elements (:fixed (:split deck)))))))
    (testing "a post opens on its cover, which moves slower than its title"
      (is (= "hero with-cover" (:class (hero tram))))
      (is (= {:src "/a/tram.jpg" :alt "A tram" :hidden false}
             (select-keys (attrs-of post {:cover tram} "img.hero-picture") [:src :alt :hidden])))
      (is (= ["0.5" "1"] (keep (comp :data-speed second) (elements (:fixed post))))))
    (testing "the parallax moves the cover through its style, so the hero hands the cover its name"
      (is (= "--cover-name: cover-lisbon" (:style (attrs-of post {:folder "lisbon"} "x-scroll-parallax")))))
    (testing "a post with no cover opens on its title alone, and shows no cover below it"
      (is (= "hero" (:class (hero nil))))
      (is (true? (:hidden (attrs-of post {:cover nil} "img.hero-picture"))))
      (is (not-any? (comp #{"post-cover"} :class second) (elements (:fixed post)))))))

(deftest essay-test
  (let [essay   (templates/named :essay)
        entries (get-in essay [:lists :main :rows "entries"])
        entry   (get-in entries [:lists :entries :row :split])
        post    (:split (get-in essay [:lists :main :rows "post"]))
        tram    {:src "/a/tram.jpg" :alt "A tram"}]
    (testing "a post in a list has its picture with its title over it, or its title alone"
      (is (= "essay-entry with-cover" (:class (attrs-of entry {:picture "/a/tram.jpg"} "li"))))
      (is (= "essay-entry" (:class (attrs-of entry {} "li"))))
      (is (false? (:gradient (attrs-of entry {:gradient-titles? true :picture "/a/tram.jpg"} "x-typography.title"))))
      (is (true? (:gradient (attrs-of entry {:gradient-titles? true} "x-typography.title")))))
    (testing "a post opens on its cover across the page, above its text and its contents"
      (is (= "hero with-cover" (:class (attrs-of post {:cover tram} "div"))))
      (is (= {:src "/a/tram.jpg" :alt "A tram" :hidden false}
             (select-keys (attrs-of post {:cover tram} "img.hero-picture") [:src :alt :hidden])))
      (let [[_ {:keys [class]} _hero [_ layout]] (:fixed post)]
        (is (= "post-page" class))
        (is (= "post-layout" (:class layout)))))
    (testing "a post with no cover opens on its title alone"
      (is (= "hero" (:class (attrs-of post {:cover nil} "div"))))
      (is (true? (:hidden (attrs-of post {:cover nil} "img.hero-picture")))))))

(deftest post-cover-test
  (doseq [template-name (remove #{:longform :essay} (keys templates/by-name))
          :let          [main (get-in (templates/named template-name) [:lists :main])
                         post (:split (templates/row-node main "post"))
                         tram {:src "/a/tram.jpg" :alt "A tram" :width 1200 :height 800}]]
    (testing (str template-name ": a post shows its cover, with its alt text and its ratio")
      (is (= {:src "/a/tram.jpg" :alt "A tram" :ratio "1200:800"}
             (select-keys (attrs-of post {:cover tram} "x-image") [:src :alt :ratio])))
      (is (false? (:hidden (attrs-of post {:cover tram} "figure.post-cover")))))
    (testing (str template-name ": a post with no cover hides its place")
      (is (true? (:hidden (attrs-of post {:cover nil} "figure.post-cover")))))))

(defn- styles
  "The styles that `split` writes for `item`."
  [split item]
  (set (keep (comp :style :attrs second) (template/writes split item))))

(def ^:private cover-style "view-transition-name: cover-lisbon; view-transition-class: cover")
(def ^:private title-style "view-transition-name: title-lisbon; view-transition-class: title")

(defn- main-row
  "The node of the row `row-name` in the main part of the blog template `template-name`."
  [template-name row-name]
  (templates/row-node (get-in (templates/named template-name) [:lists :main]) row-name))

(deftest transition-name-test
  (doseq [template-name (keys templates/by-name)
          :let          [entry (get-in (main-row template-name "entries") [:lists :entries :row :split])]]
    (testing (str template-name ": a post in a list names its picture and its title by its folder")
      (is (= #{cover-style title-style} (styles entry {:folder "lisbon"})))))
  (doseq [template-name (remove #{:longform} (keys templates/by-name))
          :let          [post (:split (main-row template-name "post"))]]
    (testing (str template-name ": the page of a post gives its cover and its title the same names")
      (is (= #{cover-style title-style} (styles post {:folder "lisbon"}))))))

(deftest series-test
  (let [post (:split (main-row :journal "post"))
        part (get-in (main-row :journal "post") [:lists :parts :row :split])
        item {:series {:name "Trip" :number 2 :count 3 :next {:title "Third" :href "/c/"}}}]
    (testing "the series of a post folds over its parts, with its name and where the post stands"
      (is (= "Trip \u00b7 Part 2 of 3" (:header (attrs-of post item "x-collapse.series")))))
    (testing "beside it is the next part, by its title, and the last part has none"
      (is (= {:href "/c/" :hidden false :aria-label "Next part: Third"}
             (select-keys (attrs-of post item "a.series-next") [:href :hidden :aria-label])))
      (is (= "Third" (text-of post item "a.series-next")))
      (is (true? (:hidden (attrs-of post (assoc-in item [:series :next] nil) "a.series-next")))))
    (testing "a part is a step on the path of the series: read, being read, or to come"
      (is (= {:label "Part 2" :status "active"}
             (select-keys (attrs-of part {:number 2 :current? true} "x-timeline-item") [:label :status])))
      (is (= "complete" (:status (attrs-of part {:read? true} "x-timeline-item"))))
      (is (= "pending" (:status (attrs-of part {} "x-timeline-item")))))
    (testing "the link of the part that is being read says it is the page"
      (is (= "page" (:aria-current (attrs-of part {:current? true} "a"))))
      (is (nil? (:aria-current (attrs-of part {} "a")))))))

(deftest diary-test
  (let [entries  (:split (get-in (templates/named :diary) [:lists :main :rows "entries"]))
        listing  {:heading nil :number 1 :pages 1 :filtered? false :count 2 :date nil
                  :days ["2026-09-29" "2026-10-02"] :language "nl"}
        calendar (fn [item] (attrs-of entries (merge listing item) "x-calendar.calendar"))]
    (testing "the calendar shows the month of the newest post, held to the days of the posts"
      (is (= {:month "2026-10" :min "2026-09-29" :max "2026-10-02" :value nil :hidden false}
             (select-keys (calendar {}) [:month :min :max :value :hidden]))))
    (testing "the calendar names its months and days in the language of the blog"
      (is (= "nl" (:locale (calendar {})))))
    (testing "every day between the posts that has no post cannot be picked"
      (is (= "2026-09-30 2026-10-01" (:disabled-dates (calendar {})))))
    (testing "a day that the reader chose is picked, and its month shown"
      (is (= {:value "2026-09-29" :month "2026-09"}
             (select-keys (calendar {:date "2026-09-29"}) [:value :month]))))
    (testing "a month that the reader chose is shown, and no day is picked"
      (is (= {:value nil :month "2026-09"} (select-keys (calendar {:date "2026-09"}) [:value :month]))))
    (testing "the page of a tag has no calendar, since a date picks nothing there"
      (is (true? (:hidden (calendar {:heading "travel"})))))
    (testing "a blog with no post that was published has no calendar, since there is no day to pick"
      (is (true? (:hidden (calendar {:days []})))))))

(deftest notebook-test
  (let [notebook (templates/named :notebook)
        item     (fn [choice] (:item (view/page "/" (site-of :notebook) (:post pages) choice)))
        written  (fn [named choice] (attrs-of (:split notebook) (item choice) named))]
    (testing "the sidebar is open when the menu is, and is docked beside the page by itself"
      (is (false? (:open (written "x-sidebar" {}))))
      (is (true? (:open (written "x-sidebar" {:menu? true})))))
    (testing "the palette is open when the reader opened it"
      (is (false? (:open (written "x-command-palette" {}))))
      (is (true? (:open (written "x-command-palette" {:palette? true})))))
    (testing "the button of the palette names its key as the keyboard of the reader does"
      (is (= "\u2318K" (text-of (:split notebook) (item {:command-key? true}) "kbd")))
      (is (= "Ctrl K" (text-of (:split notebook) (item {})  "kbd"))))
    (testing "the way back to every post shows once a tag is chosen"
      (is (true? (:hidden (written "button.clear" {}))))
      (is (false? (:hidden (written "button.clear" {:tag "travel"})))))
    (testing "the palette is given its commands as a property, each under the name of its group"
      (let [commands [{:id "b" :year "2026"} {:id "n"} {:id "about" :page? true}]
            writes   (into {} (template/writes (:split notebook) (assoc (item {}) :commands commands)))]
        (is (= [{:id "b" :group "2026"} {:id "n" :group "No date"} {:id "about" :group "Pages"}]
               (get-in writes ["palette" :props :items])))))
    (testing "a row of the sidebar marks the post of the page, and shows its year when it is the first"
      (let [row (get-in notebook [:lists :posts :row :split])]
        (is (= "page" (:aria-current (attrs-of row {:current? true :href "/a/"} "a"))))
        (is (nil? (:aria-current (attrs-of row {:current? false :href "/a/"} "a"))))))
    (testing "both theme selects list the presets of the blog"
      (is (= [:presets :presets]
             (mapv (fn [[list-name row-list]] (:of row-list list-name))
                   (select-keys (:lists notebook) [:presets :menu-presets])))))))

(defn- link-attrs
  "The attributes of each link in the list `list-name` of the document of `template-name`,
   for the page `told`."
  [template-name list-name told]
  (let [row-list (get-in (templates/named template-name) [:lists list-name])
        rows     (templates/rows-of row-list list-name (view/page "/" (site-of template-name) told {}))]
    (for [k (:order rows)]
      (attrs-of (:split (:row row-list)) (:item ((:by-key rows) k)) "a.nav-link"))))

(defn- badge-hidden?
  "Whether the page of the author in `folder` hides its badge, in the blog template
   `template-name`."
  [template-name folder]
  (let [told {:page {:kind :standalone :folder folder} :text {:folder folder :title "T" :blocks []}}
        page (view/page "/" (site-of template-name) told {})
        node (templates/row-node (get-in (templates/named template-name) [:lists :main]) "standalone")]
    (:hidden (attrs-of (:split node) (get-in page [:lists :main :by-key "standalone" :item]) "x-badge.draft"))))

(deftest standalone-test
  (doseq [template-name (keys templates/by-name)]
    (testing (str template-name ": the links of the pages are in the navbar and in the menu, the same in both")
      (is (= [{:href "/about/" :aria-current nil} {:href "/contact/" :aria-current nil}]
             (link-attrs template-name :pages (:archive pages))
             (link-attrs template-name :menu-pages (:archive pages)))))
    (testing (str template-name ": the link to the page that is shown says so")
      (is (= ["page" nil] (map :aria-current (link-attrs template-name :pages (:standalone pages))))))
    (testing (str template-name ": a page that is not published says so, and one that is does not")
      (is (false? (badge-hidden? template-name "about")))
      (is (true? (badge-hidden? template-name "contact"))))
    (testing (str template-name ": a page is named by its title in the title of the document")
      (is (= "About · Bare notes"
             (:title (view/page "/" (site-of template-name) (:standalone pages) {})))))
    (testing (str template-name ": a page opens with its article, as a post does, with no backdrop of its own")
      (is (= :article (first (:fixed (:split (templates/row-node (get-in (templates/named template-name) [:lists :main])
                                                                 "standalone")))))))))

(defn- home-links
  "The elements in the tree `tree` that lead to the home page."
  [tree]
  (filter (fn [x] (and (vector? x) (map? (second x)) (= :home (:href (second x)))))
          (tree-seq vector? seq tree)))

(deftest home-link-test
  (testing "in the navbar the brand leads to the home page, and nothing else does"
    (is (= [pages/brand] (home-links (pages/navbar pages/search-field)))))
  (testing "the links of the navbar, which a menu holds too, have none to the home page"
    (is (empty? (home-links (pages/links "menu-pages"))))))

(defn- feed-links
  "The elements of the document of `template-name` that lead to the feed of a blog at `root`,
   each as its fixed attributes."
  [template-name root]
  (let [split   (:split (templates/named template-name))
        item    (:item (view/page root (site-of template-name) (:archive pages) {}))
        leading (into #{}
                      (keep (fn [[part-name write]]
                              (when (= (str root wire/feed) (get-in write [:attrs :href])) part-name)))
                      (template/writes split item))]
    (for [[_tag attrs] (elements (:fixed split))
          :when        (contains? leading (:data-x-part attrs))]
      (dissoc attrs :data-x-part))))

(deftest feed-link-test
  (doseq [template-name (keys templates/by-name)]
    (testing (str template-name ": the navbar and the menu each lead to the feed, under the top of the blog")
      (is (= [{:class "nav-link" :type wire/feed-type} {:class "nav-link" :type wire/feed-type}]
             (feed-links template-name "/my-blog/"))))))

(defn- footer-hidden?
  "Whether the footer of the document of `template-name` is hidden, for a blog with the
   settings `settings` besides those of the site."
  [template-name settings]
  (let [split  (:split (templates/named template-name))
        footer (some (fn [[tag attrs]] (when (= :footer tag) (:data-x-part attrs)))
                     (elements (:fixed split)))
        site   (update (site-of template-name) :blog merge settings)
        item   (:item (view/page "/" site (:archive pages) {}))]
    (get-in (into {} (template/writes split item)) [footer :attrs :hidden])))

(defn- brand-writes
  "What the brand of the document of `template-name` is written with, by the tag of each of its
   elements, for a blog with the pictures `pictures`."
  [template-name pictures]
  (let [split  (:split (templates/named template-name))
        brand  (some (fn [[_ attrs :as piece]] (when (= "brand" (:class attrs)) piece))
                     (elements (:fixed split)))
        parts  (into {} (keep (fn [[tag attrs]] (some->> (:data-x-part attrs) (vector tag))))
                     (elements (into [:a] (last brand))))
        site   (assoc (site-of template-name) :pictures pictures)
        writes (into {} (template/writes split (:item (view/page "/" site (:archive pages) {}))))]
    (update-vals parts writes)))

(deftest brand-test
  (doseq [template-name (keys templates/by-name)]
    (testing (str template-name ": without a logo, the brand is the name of the blog")
      (let [{:keys [picture source span]} (brand-writes template-name nil)]
        (is (true? (get-in picture [:attrs :hidden])))
        (is (nil? (get-in source [:attrs :srcset])))
        (is (= {:attrs {:hidden false} :text "Bare notes"} span))))
    (testing (str template-name ": a logo is the whole brand, with the name as its text, and a
                  picture for the dark mode where there is one")
      (let [{:keys [picture source img span]}
            (brand-writes template-name {:logo {:light {:src "logo.svg" :version "a1"}
                                                :dark  {:src "logo dark.svg" :version "b2"}}})]
        (is (false? (get-in picture [:attrs :hidden])))
        (is (= {:src "/logo.svg?v=a1" :alt "Bare notes"} (:attrs img)))
        (is (= "/logo%20dark.svg?v=b2" (get-in source [:attrs :srcset])))
        (is (true? (get-in span [:attrs :hidden])))))
    (testing (str template-name ": a logo with one picture shows it in both modes")
      (is (nil? (get-in (brand-writes template-name {:logo {:light {:src "logo.svg" :version "a1"}}})
                        [:source :attrs :srcset]))))))

(deftest favicon-test
  (testing "a blog with no favicon has no tag of one"
    (is (= [] (pages/favicon-tags (:favicon (view/page "/" site (:archive pages) {}))))))
  (testing "a favicon is a tag of the head, and one for the dark mode follows it"
    (is (= [{:href "/my-blog/icon.svg?v=a1"}
            {:href "/my-blog/icon-dark.svg?v=b2" :media "(prefers-color-scheme: dark)"}]
           (pages/favicon-tags (:favicon (view/page "/my-blog/"
                                             (assoc site :pictures {:favicon {:light {:src "icon.svg" :version "a1"}
                                                                              :dark  {:src "icon-dark.svg" :version "b2"}}})
                                             (:archive pages) {})))))))

(deftest footer-test
  (doseq [template-name (keys templates/by-name)]
    (testing (str template-name ": every page ends with what made the blog, which leads to BareBlog")
      (is (= [[:footer {:class "site-footer"} ["Powered by " [:a {:href pages/bareblog-address} "BareBlog"]]]]
             (for [[tag attrs children] (elements (:fixed (:split (templates/named template-name))))
                   :when                (= :footer tag)]
               [tag (dissoc attrs :data-x-part) children]))))
    (testing (str template-name ": the settings of a blog can leave it out")
      (is (false? (footer-hidden? template-name {})))
      (is (false? (footer-hidden? template-name {:powered-by-footer true})))
      (is (true? (footer-hidden? template-name {:powered-by-footer false}))))))

(def ^:private shell-elements
  "The custom elements of the document of each blog template, on a page that shows nothing."
  {:journal  ["x-container" "x-drawer" "x-gaussian-blur" "x-navbar" "x-search-field" "x-select" "x-theme"]
   :notebook ["x-badge" "x-command-palette" "x-container" "x-gaussian-blur" "x-navbar" "x-select" "x-sidebar"
              "x-theme"]
   :magazine ["x-container" "x-drawer" "x-gaussian-blur" "x-navbar" "x-search-field" "x-select" "x-theme"]
   :longform ["x-container" "x-drawer" "x-gaussian-blur" "x-navbar" "x-search-field" "x-select" "x-theme"]
   :diary    ["x-container" "x-drawer" "x-gaussian-blur" "x-navbar" "x-search-field" "x-select" "x-theme"]
   :essay    ["x-container" "x-drawer" "x-gaussian-blur" "x-navbar" "x-search-field" "x-select" "x-theme"]})

(def ^:private post-elements
  "The custom elements that the page of a post has in a blog template, besides those of its
   blocks."
  {:longform ["x-scroll-parallax"]})

(deftest custom-elements-test
  (doseq [[template-name shell] shell-elements
          :let                  [tags-of (fn [told] (set (templates/custom-elements
                                                          (templates/named template-name)
                                                          (view/page "/" (site-of template-name) told {}))))]]
    (testing (str template-name ": a page that shows nothing has the custom elements of the document")
      (is (= (set shell) (tags-of (:not-found pages)))))
    (testing (str template-name ": a page has those of what it shows too")
      (is (= (conj (set shell) "x-badge") (tags-of (:archive pages)))))
    (testing (str template-name ": the page of a post has those of its blocks, at any depth")
      (is (= (-> (set shell)
                 (into ["x-badge" "x-card" "x-code" "x-collapse" "x-image" "x-progress" "x-table"
                       "x-table-cell" "x-table-row" "x-timeline" "x-toc" "x-typography"])
                 (into (post-elements template-name)))
             (tags-of (:post pages))))))
  (testing "the page of a post in a series has the steps of the path of its parts"
    (is (every? (templates/custom-elements (templates/named :journal)
                                           (view/page "/" (assoc site :series {"Trip" ["a" "b"]}) (:post pages) {}))
                ["x-timeline-item"])))
  (testing "a home page of entries has its page numbers"
    (is (contains? (templates/custom-elements (templates/named :journal) (view/page "/" site (:home pages) {}))
                   "x-pagination")))
  (testing "a home page of Magazine has its grid of cards"
    (is (every? (templates/custom-elements (templates/named :magazine)
                                           (view/page "/" (site-of :magazine) (:home pages) {}))
                ["x-bento-grid" "x-bento-item" "x-card" "x-pagination"]))))

(def ^:private page-events
  "The events of the pages, which every blog template has."
  #{["page-change-request" :to-page]
    ["click" :tag]
    ["click" :tag-link]
    ["click" :clear]})

(def ^:private shell-events
  "The events of the document of each blog template."
  {:journal  #{["x-search-field-change-request" :search]
               ["x-search-field-search" :submit-search]
               ["x-select-change-request" :preset]
               ["click" :open-menu]
               ["click" :close-menu]
               ["x-drawer-toggle" :menu]}
   :notebook #{["x-command-palette-open-request" :open-palette]
               ["x-command-palette-close-request" :close-palette]
               ["x-command-palette-select" :open-post]
               ["x-select-change-request" :preset]
               ["click" :open-palette]
               ["click" :open-menu]
               ["toggle" :menu]}
   :magazine #{["x-search-field-change-request" :search]
               ["x-search-field-search" :submit-search]
               ["x-select-change-request" :preset]
               ["click" :open-menu]
               ["click" :close-menu]
               ["x-drawer-toggle" :menu]}
   :longform #{["x-search-field-change-request" :search]
               ["x-search-field-search" :submit-search]
               ["x-select-change-request" :preset]
               ["click" :open-menu]
               ["click" :close-menu]
               ["x-drawer-toggle" :menu]}
   :diary    #{["x-search-field-change-request" :search]
               ["x-search-field-search" :submit-search]
               ["x-select-change-request" :preset]
               ["click" :open-menu]
               ["click" :close-menu]
               ["x-drawer-toggle" :menu]
               ["x-calendar-change" :day]
               ["x-calendar-navigate" :month]}
   :essay    #{["x-search-field-change-request" :search]
               ["x-search-field-search" :submit-search]
               ["x-select-change-request" :preset]
               ["click" :open-menu]
               ["click" :close-menu]
               ["x-drawer-toggle" :menu]}})

(deftest every-template-is-tested-test
  (testing "the tables of elements and of events have every blog template"
    (is (= (set (keys templates/by-name)) (set (keys shell-elements)) (set (keys shell-events))))))

(deftest events-test
  (doseq [[template-name shell] shell-events]
    (testing (str "the events of " template-name " are the ones its templates name, by event type and part")
      (is (= (into page-events shell)
             (set (map (fn [[[event-type _part] meaning]] [event-type meaning])
                       (templates/events (templates/named template-name)))))))))

(deftest facts-test
  (testing "the lists of a document are named once, also when two parts show the same rows"
    (is (= #{:main :presets :pages} (templates/list-names (templates/named :journal))))
    (is (= #{:main :presets :pages :tags :posts} (templates/list-names (templates/named :notebook)))))
  (testing "the blog template of a blog is the one its settings name, or the default"
    (is (= (templates/named :notebook) (templates/of-blog {:template :notebook})))
    (is (= (templates/named shape/default-template) (templates/of-blog {})))))
