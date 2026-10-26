(ns bareblog.app.view-test
  (:require [bareblog.app.view :as view]
            [clojure.test :refer [deftest is testing]]))

(def blog {:title "Bare notes" :template :journal :available-themes ["ocean" "forest"] :per-page 2})

(def posts
  [{:folder "c" :title "Third" :date "2026-03-01T21:15:00+01:00" :tags ["travel" "food"] :summary "Three."
    :picture "./tram.png"}
   {:folder "b" :title "Second" :date "2026-02-01" :tags ["travel"] :summary "Two." :draft? true}
   {:folder "a" :title "First" :date "2025-01-01" :summary "One."}])

(def site {:blog blog :posts posts})

(def days
  "The days of the posts of `site`, oldest first."
  ["2025-01-01" "2026-02-01" "2026-03-01"])

(defn- shown
  "The key and the node of what the page `told` shows of `site`, whose top is at `root`, for
   what is `local` to the page."
  ([told] (shown site told))
  ([site told] (shown "/" site told))
  ([root site told] (shown root site told {}))
  ([root site told local]
   (let [{:keys [order by-key]} (get-in (view/page root site told local) [:lists :main])]
     [(first order) (by-key (first order))])))

(defn- row-items
  "The item of each row of the list `list-name` of `node`, in order."
  [node list-name]
  (let [{:keys [order by-key]} (get-in node [:lists list-name])]
    (mapv (comp :item by-key) order)))

(defn- block-list
  "The blocks of `node`, in order."
  [node]
  (let [{:keys [order by-key]} (get-in node [:lists :blocks])]
    (mapv by-key order)))

(deftest document-test
  (let [page (view/page "/" site {:page {:kind :archive}} {})]
    (testing "the page names the blog template and the language of the blog"
      (is (= :journal (:template page)))
      (is (= "en" (:language page)))
      (is (= "nl" (:language (view/page "/" (assoc-in site [:blog :language] "nl") {:page {:kind :archive}} {})))))
    (testing "the document is given the preset, the name of the blog and the addresses of its links"
      (is (= {:preset "ocean" :presets 2 :query "" :blog-title "Bare notes" :home "/" :archive "/archive/"
              :feed "/feed.xml"}
             (select-keys (:item page) [:preset :presets :query :blog-title :home :archive :feed]))))
    (testing "a blog under a path has its feed under that path"
      (is (= "/my-blog/feed.xml" (get-in (view/page "/my-blog/" site {:page {:kind :archive}} {}) [:item :feed]))))
    (testing "nothing is open until the page is told otherwise"
      (is (= {:menu? false :palette? false :command-key? false}
             (select-keys (:item page) [:menu? :palette? :command-key?]))))
    (testing "the document is given the lists that its blog template asks for, and no other"
      (is (= #{:main :presets :pages} (set (keys (:lists page)))))
      (is (not (contains? (:item page) :commands))))
    (testing "the document lists the themes that the blog offers, as the presets to choose from"
      (is (= ["ocean" "forest"] (get-in page [:lists :presets :order]))))
    (testing "the document holds one page"
      (is (= 1 (count (get-in page [:lists :main :order])))))
    (testing "a blog that names no blog template gets the default, and no preset is none"
      (let [bare (view/page "/" {:blog {:title "Bare"} :posts []} {:page {:kind :archive}} {})]
        (is (= :journal (:template bare)))
        (is (nil? (get-in bare [:item :preset])))))))

(deftest title-test
  (testing "the first page of the home page has the name of the blog"
    (is (= "Bare notes" (:title (view/page "/" site {:page {:kind :home :number 1}} {})))))
  (testing "every other page names itself first"
    (is (= "Page 2 · Bare notes" (:title (view/page "/" site {:page {:kind :home :number 2}} {}))))
    (is (= "travel · Bare notes" (:title (view/page "/" site {:page {:kind :tag :tag "travel"}} {}))))
    (is (= "Archive · Bare notes" (:title (view/page "/" site {:page {:kind :archive}} {}))))
    (is (= "1 post found · Bare notes" (:title (view/page "/" site {:page {:kind :home :number 1}} {:query "third"}))))
    (is (= "No posts found · Bare notes" (:title (view/page "/" site {:page {:kind :home :number 2}} {:query "zebra"}))))
    (is (= "Third · Bare notes"
           (:title (view/page "/" site {:page {:kind :post :folder "c"} :text {:folder "c" :title "Third" :blocks []}} {}))))))

(defn- first-picture
  "The picture of the first entry of the home page, when the first post has `picture`, in a
   blog whose top is at `root`."
  [root picture]
  (let [[_ node] (shown root (assoc-in site [:posts 0 :picture] picture) {:page {:kind :home :number 1}})]
    (:picture (first (row-items node :entries)))))

(deftest home-test
  (let [[k node] (shown {:page {:kind :home :number 1}})]
    (testing "the home page is a page of entries with no heading"
      (is (= "entries" k))
      (is (= {:heading nil :count 2 :number 1 :pages 2 :filtered? false}
             (select-keys (:item node) [:heading :count :number :pages :filtered?]))))
    (testing "the list knows the days of the posts and the language of the blog"
      (is (= {:days days :language "en"} (select-keys (:item node) [:days :language]))))
    (testing "it offers every tag of the blog to choose from, and none is chosen"
      (is (= [{:name "food" :chosen? false} {:name "travel" :chosen? false}] (row-items node :all-tags))))
    (testing "it shows the first posts of the list, as many as the blog asks for on a page"
      (is (= ["c" "b"] (get-in node [:lists :entries :order]))))
    (testing "an entry shows the folder, the title, the date, the summary and the address of its
              post, and its picture at the address of its file"
      (is (= {:folder "c" :title "Third" :date "2026-03-01" :href "/c/" :draft? false :summary "Three."
              :picture "/c/tram.png" :front? true :first-picture? true :gradient-titles? false}
             (first (row-items node :entries)))))
    (testing "the entries of the first page are on the front, and those of any other page are not"
      (is (every? :front? (row-items node :entries)))
      (let [[_ second-page] (shown {:page {:kind :home :number 2}})]
        (is (not-any? :front? (row-items second-page :entries)))))
    (testing "the first entry of a page with a picture is the first picture, and no other is"
      (let [first-pictures (fn [posts]
                             (let [[_ page] (shown (assoc site :posts posts) {:page {:kind :home :number 1}})]
                               (map :first-picture? (row-items page :entries))))]
        (is (= [true false] (first-pictures posts)))
        (is (= [false true] (first-pictures [(dissoc (first posts) :picture)
                                             (assoc (second posts) :picture "a.png")])))
        (is (= [false false] (first-pictures [(dissoc (first posts) :picture) (second posts)])))))
    (testing "an entry with no picture shows none"
      (is (nil? (:picture (second (row-items node :entries))))))
    (testing "a picture at a full address keeps it, and one under a path gets that path"
      (is (= "https://cdn.example.com/a.gif" (first-picture "/" "https://cdn.example.com/a.gif")))
      (is (= "/notes/c/tram.png" (first-picture "/notes/" "tram.png"))))
    (testing "a draft says that it is one"
      (is (true? (:draft? (second (row-items node :entries))))))
    (testing "an entry lists its tags, each with the address of its page"
      (is (= [{:name "travel" :href "/tags/travel/"} {:name "food" :href "/tags/food/"}]
             (row-items (get-in node [:lists :entries :by-key "c"]) :tags)))))
  (testing "a later page shows the posts after those"
    (is (= ["a"] (get-in (second (shown {:page {:kind :home :number 2}})) [:lists :entries :order]))))
  (testing "a page past the last one is not found"
    (is (= "not-found" (first (shown {:page {:kind :home :number 3}})))))
  (testing "a blog that asks for nothing gets ten on a page, on one page"
    (let [[_ node] (shown (update site :blog dissoc :per-page) {:page {:kind :home :number 1}})]
      (is (= {:pages 1 :count 3} (select-keys (:item node) [:pages :count])))
      (is (= 3 (count (get-in node [:lists :entries :order]))))))
  (testing "a blog with no posts has a home page with no entries"
    (is (= ["entries" 0 []] (let [[k node] (shown {:blog blog :posts []} {:page {:kind :home :number 1}})]
                              [k (get-in node [:item :count]) (get-in node [:lists :entries :order])])))))

(deftest date-test
  (let [found (fn [date]
                (let [{:keys [order by-key]} (get-in (view/page "/" site {:page {:kind :home :number 1}}
                                                                 {:date date})
                                                     [:lists :main])
                      node (by-key (first order))]
                  {:order (get-in node [:lists :entries :order]) :item (:item node)}))]
    (testing "a month shows the posts of that month, as a choice"
      (is (= ["b"] (:order (found "2026-02"))))
      (is (= {:filtered? true :date "2026-02"} (select-keys (:item (found "2026-02")) [:filtered? :date]))))
    (testing "a day shows the posts of that day"
      (is (= ["c"] (:order (found "2026-03-01")))))
    (testing "a date with no post shows none"
      (is (empty? (:order (found "2026-04")))))))

(deftest tag-test
  (let [[k node] (shown {:page {:kind :tag :tag "travel"}})]
    (testing "the page of a tag is a page of entries under the tag, on one page"
      (is (= "entries" k))
      (is (= {:heading "travel" :count 2 :number 1 :pages 1 :filtered? false}
             (select-keys (:item node) [:heading :count :number :pages :filtered?]))))
    (testing "it offers no tags to choose from"
      (is (= [] (row-items node :all-tags))))
    (testing "it shows every post with the tag"
      (is (= ["c" "b"] (get-in node [:lists :entries :order])))))
  (testing "a tag that no post has is not found"
    (is (= "not-found" (first (shown {:page {:kind :tag :tag "nothing"}}))))))

(deftest archive-test
  (let [[k node] (shown {:page {:kind :archive}})]
    (testing "the archive shows every post"
      (is (= "archive" k))
      (is (= ["c" "b" "a"] (get-in node [:lists :rows :order]))))
    (testing "the archive says how many posts it shows"
      (is (= {:count 3} (:item node))))
    (testing "a row has its year, and is the first of its year when the row before it has another"
      (is (= [["2026" true] ["2026" false] ["2025" true]]
             (mapv (juxt :year :first-of-year?) (row-items node :rows)))))
    (testing "a row shows the title, the date and the address of its post"
      (is (= {:title "First" :date "2025-01-01" :href "/a/" :draft? false :year "2025" :first-of-year? true}
             (last (row-items node :rows))))))
  (testing "a post with no date has no year, and the first of them is the first of its year"
    (let [undated {:blog blog :posts [{:folder "a" :title "A" :date "2026-01-01"}
                                      {:folder "b" :title "B"}
                                      {:folder "c" :title "C"}]}]
      (is (= [["2026" true] [nil true] [nil false]]
             (mapv (juxt :year :first-of-year?) (row-items (second (shown undated {:page {:kind :archive}})) :rows))))))
  (testing "the first row is the first of its year, also when it has no date"
    (is (= [[nil true]]
           (mapv (juxt :year :first-of-year?)
                 (row-items (second (shown {:blog blog :posts [{:folder "b" :title "B"}]} {:page {:kind :archive}})) :rows))))))

(defn- chosen
  "The key and the node of the home page for what the reader chose in `choice`."
  [choice]
  (let [{:keys [order by-key]} (get-in (view/page "/" site {:page {:kind :home :number 1}} choice) [:lists :main])]
    [(first order) (by-key (first order))]))

(defn- found [choice]
  (get-in (second (chosen choice)) [:lists :entries :order]))

(deftest search-test
  (testing "a search shows every post that matches, on one page, and says that it is a choice"
    (let [[k node] (chosen {:query "t"})]
      (is (= "entries" k))
      (is (= {:heading nil :count 3 :number 1 :pages 1 :filtered? true}
             (select-keys (:item node) [:heading :count :number :pages :filtered?])))))
  (testing "a word is looked for in the title, the summary and the tags, in any case"
    (is (= ["c"] (found {:query "THIRD"})))
    (is (= ["a"] (found {:query "one."})))
    (is (= ["c"] (found {:query "food"}))))
  (testing "a letter is found with or without its accent, either way round"
    (let [accented (assoc site :posts [{:folder "c" :title "Café Lisboa" :summary "Pastéis."}])
          found-in (fn [query] (get-in (view/page "/" accented {:page {:kind :home :number 1}} {:query query})
                                       [:lists :main :by-key "entries" :lists :entries :order]))]
      (is (= ["c"] (found-in "cafe")))
      (is (= ["c"] (found-in "CAFÉ")))
      (is (= ["c"] (found-in "pasteis")))))
  (testing "a word is found inside a longer word"
    (is (= ["c"] (found {:query "hir"}))))
  (testing "every word must be there, and each may be in another place"
    (is (= ["b"] (found {:query "second travel"})))
    (is (= [] (found {:query "second food"}))))
  (testing "a search for nothing but spaces is no search"
    (is (= ["c" "b"] (found {:query "   "})))
    (is (false? (get-in (second (chosen {:query "   "})) [:item :filtered?]))))
  (testing "a search that finds nothing shows no posts, and is still the page of entries"
    (is (= ["entries" 0] (let [[k node] (chosen {:query "zebra"})] [k (get-in node [:item :count])]))))
  (testing "a search looks through every post, also from a later page"
    (is (= ["a"] (get-in (view/page "/" site {:page {:kind :home :number 2}} {:query "first"})
                         [:lists :main :by-key "entries" :lists :entries :order])))))

(deftest tag-choice-test
  (testing "a chosen tag shows the posts that have it"
    (is (= ["c" "b"] (found {:tag "travel"})))
    (is (= ["c"] (found {:tag "food"}))))
  (testing "the chosen tag is marked among the tags to choose from"
    (is (= [{:name "food" :chosen? false} {:name "travel" :chosen? true}]
           (row-items (second (chosen {:tag "travel"})) :all-tags))))
  (testing "a tag and a search together show the posts that pass both"
    (is (= ["b"] (found {:tag "travel" :query "second"}))))
  (testing "a choice changes nothing on a page that is not the home page"
    (is (= ["c" "b"] (get-in (view/page "/" site {:page {:kind :tag :tag "travel"}} {:query "third" :tag "food"})
                             [:lists :main :by-key "entries" :lists :entries :order])))))

(deftest preset-test
  (let [preset-of (fn [site choice] (get-in (view/page "/" site {:page {:kind :archive}} choice) [:item :preset]))]
    (testing "the preset is the first that the blog offers, until the reader chooses one"
      (is (= "ocean" (preset-of site {})))
      (is (= "forest" (preset-of site {:preset "forest"}))))
    (testing "a preset that the blog does not offer is not chosen"
      (is (= "ocean" (preset-of site {:preset "sunset"}))))
    (testing "the page shows the text that is searched for in its search field"
      (is (= "lisbon" (get-in (view/page "/" site {:page {:kind :archive}} {:query "lisbon"}) [:item :query]))))))

(def one {:kind :text :content ["One."]})
(def two {:kind :text :content ["Two."]})

(defn- post-node
  ([folder post] (post-node folder post {}))
  ([folder post local]
   (second (shown "/" site {:page {:kind :post :folder folder} :text (assoc post :folder folder)}
                  local))))

(deftest post-series-test
  (let [site   (assoc site :series {"Trip" ["a" "b" "c"]})
        node   (fn [folder]
                 (second (shown site {:page {:kind :post :folder folder}
                                      :text {:folder folder :title "x" :blocks []}})))]
    (testing "a post shows the series it is a part of: its name, its number, the count and the
              next part"
      (is (= {:name "Trip" :number 2 :count 3 :next {:title "Third" :href "/c/"}}
             (:series (:item (node "b"))))))
    (testing "the parts are in the order of the series, marked as read, being read, or to come"
      (is (= [{:title "First" :href "/a/" :number 1 :current? false :read? true}
              {:title "Second" :href "/b/" :number 2 :current? true :read? false}
              {:title "Third" :href "/c/" :number 3 :current? false :read? false}]
             (row-items (node "b") :parts))))
    (testing "the last part has no next part"
      (is (nil? (:next (:series (:item (node "c"))))))))
  (testing "a post in no series shows none, and no parts"
    (let [node (post-node "a" {:title "x" :blocks []})]
      (is (nil? (:series (:item node))))
      (is (= [] (row-items node :parts))))))

(deftest post-test
  (let [node (post-node "b" {:title "Second" :date "2026-02-01T08:00:00+01:00" :tags ["travel" "travel"] :blocks [one two]})]
    (testing "the page of a post shows its folder, its title and its date, and that it is a draft"
      (is (= {:folder "b" :title "Second" :date "2026-02-01" :draft? true}
             (select-keys (:item node) [:folder :title :date :draft?]))))
    (testing "it has the title and the address of the newer post and of the older one"
      (is (= {:newer {:title "Third" :href "/c/"} :older {:title "First" :href "/a/"}}
             (select-keys (:item node) [:newer :older]))))
    (testing "a tag that is named twice shows once"
      (is (= [{:name "travel" :href "/tags/travel/"}] (row-items node :tags))))
    (testing "the blocks are in the order of the post, each under its key"
      (is (= [one two] (block-list node)))))
  (let [tram {:kind :photo :src "/c/tram.jpg" :alt "A tram" :width 1200 :height 800}]
    (testing "a photo that opens a post is its cover, and not one of its blocks"
      (let [node (post-node "c" {:title "Third" :blocks [tram one]})]
        (is (= tram (:cover (:item node))))
        (is (= [one] (block-list node)))))
    (testing "a post that opens with text has no cover, and keeps its photos among its blocks"
      (let [node (post-node "c" {:title "Third" :blocks [one tram]})]
        (is (nil? (:cover (:item node))))
        (is (= [one tram] (block-list node))))))
  (testing "the page of a post has how far the reader has read it, or nothing when it fits"
    (is (= 0.4 (:read (:item (post-node "a" {:title "First" :blocks []} {:read 0.4})))))
    (is (nil? (:read (:item (post-node "a" {:title "First" :blocks []}))))))
  (testing "the newest post has no newer one, and the oldest no older one"
    (is (nil? (:newer (:item (post-node "c" {:title "Third" :blocks []})))))
    (is (nil? (:older (:item (post-node "a" {:title "First" :blocks []}))))))
  (testing "the page of a post that is not there is not found"
    (is (= "not-found" (first (shown {:page {:kind :post :folder "gone"}}))))))

(deftest keys-test
  (let [order-of (fn [blocks] (get-in (post-node "c" {:title "Third" :blocks blocks}) [:lists :blocks :order]))]
    (testing "a key is text"
      (is (every? string? (order-of [one two]))))
    (testing "the same blocks give the same keys"
      (is (= (order-of [one two]) (order-of [one two]))))
    (testing "a changed block gets a new key, and the others keep theirs"
      (let [[a b] (order-of [one two])
            [c d] (order-of [one {:kind :text :content ["Two!"]}])]
        (is (= a c))
        (is (not= b d))))
    (testing "a block keeps its key when it moves"
      (is (= (order-of [one two]) (reverse (order-of [two one])))))
    (testing "two blocks with the same content get different keys"
      (is (= 2 (count (set (order-of [one one]))))))))

(deftest not-found-test
  (testing "a page of no known kind is not found, and links home"
    (is (= ["not-found" {:item {:home "/"}}] (shown {:page {:kind :not-found}})))
    (is (= "Not found · Bare notes" (:title (view/page "/" site {:page {:kind :not-found}} {})))))
  (testing "a page that is not there has no card, and one that is has"
    (is (nil? (:card (view/page "/" site {:page {:kind :not-found}} {}))))
    (is (nil? (:card (view/page "/" site {:page {:kind :post :folder "gone"}} {}))))
    (is (= {:title "Bare notes" :kind :website :site-name "Bare notes"}
           (:card (view/page "/" site {:page {:kind :home :number 1}} {}))))
    (is (= {:title "Third" :kind :article}
           (select-keys (:card (view/page "/" site {:page {:kind :post :folder "c"} :text {:folder "c" :title "Third"}} {}))
                        [:title :kind])))))

(def notebook (assoc-in site [:blog :template] :notebook))

(def newest {:title "Third" :folder "c" :blocks [one]})

(deftest notebook-home-test
  (testing "the home page of Notebook is the page of the newest post, which the page was told"
    (let [[k node] (shown notebook {:page {:kind :home :number 1} :text newest})]
      (is (= "post" k))
      (is (= "Third" (get-in node [:item :title])))
      (is (= {:title "Second" :href "/b/"} (get-in node [:item :older])))))
  (testing "it has the name of the blog alone as its title"
    (is (= "Bare notes" (:title (view/page "/" notebook {:page {:kind :home :number 1} :text newest} {})))))
  (testing "it has no later page"
    (is (= "not-found" (first (shown notebook {:page {:kind :home :number 2} :text newest})))))
  (testing "a blog with no posts has a home page with no entries"
    (is (= "entries" (first (shown (assoc notebook :posts []) {:page {:kind :home :number 1}})))))
  (testing "a home page that was told no post, of a blog that has posts, is not found"
    (is (= "not-found" (first (shown notebook {:page {:kind :home :number 1}})))))
  (testing "a search and a tag do not change what the home page shows"
    (is (= "post" (first (get-in (view/page "/" notebook {:page {:kind :home :number 1} :text newest}
                                            {:query "first" :tag "food"})
                                 [:lists :main :order]))))))

(defn- sidebar
  "The items of the rows of every post that the page `told` of the Notebook blog lists."
  [told choice]
  (let [{:keys [order by-key]} (get-in (view/page "/" notebook told choice) [:lists :posts])]
    (mapv (comp :item by-key) order)))

(deftest sidebar-test
  (let [on-post {:page {:kind :post :folder "b"} :text {:folder "b" :title "Second" :blocks []}}]
    (testing "every page lists every post, by year, and marks the post that it shows"
      (is (= [["/c/" "2026" true false] ["/b/" "2026" false true] ["/a/" "2025" true false]]
             (mapv (juxt :href :year :first-of-year? :current?) (sidebar on-post {})))))
    (testing "the home page marks the newest post, which it shows"
      (is (= [true false false]
             (mapv :current? (sidebar {:page {:kind :home :number 1} :text newest} {})))))
    (testing "a page that shows no post marks none"
      (is (= [false false false] (mapv :current? (sidebar {:page {:kind :archive}} {})))))
    (testing "a chosen tag leaves the posts that have it, and each link carries the tag"
      (is (= ["/c/?tag=food"] (mapv :href (sidebar on-post {:tag "food"}))))
      (is (= ["/c/?tag=travel" "/b/?tag=travel"] (mapv :href (sidebar on-post {:tag "travel"})))))
    (testing "the first post that is left is the first of its year"
      (is (= [true] (mapv :first-of-year? (sidebar on-post {:tag "food"})))))
    (testing "the tags to choose from are every tag of the blog, with the chosen one marked"
      (is (= [{:name "food" :chosen? true} {:name "travel" :chosen? false}]
             (row-items (view/page "/" notebook on-post {:tag "food"}) :tags))))
    (testing "the document is given the tag that the reader chose, where the page shows a choice"
      (is (= "food" (get-in (view/page "/" notebook on-post {:tag "food"}) [:item :tag])))
      (is (nil? (get-in (view/page "/" notebook on-post {}) [:item :tag])))
      (is (nil? (get-in (view/page "/" site on-post {:tag "food"}) [:item :tag]))))
    (testing "a search does not narrow the list"
      (is (= 3 (count (sidebar on-post {:query "third"})))))
    (testing "the document of Notebook is given the posts as commands, and Journal's is not"
      (is (= #{:main :presets :pages :tags :posts} (set (keys (:lists (view/page "/" notebook on-post {}))))))
      (is (contains? (:item (view/page "/" notebook on-post {})) :commands)))))

(def with-pages
  (assoc notebook :pages [{:folder "about" :title "About" :summary "Who writes." :order 1}
                          {:folder "contact" :title "Contact" :unpublished? true}]))

(deftest commands-test
  (testing "every post is a command: what is shown, what is searched, where it leads, and its year"
    (is (= [{:id "c" :label "Third" :year "2026" :keywords ["Three." "travel" "food"] :value "/c/"}
            {:id "b" :label "Second" :year "2026" :keywords ["Two." "travel"] :value "/b/"}
            {:id "a" :label "First" :year "2025" :keywords ["One."] :value "/a/"}]
           (get-in (view/page "/" notebook {:page {:kind :archive}} {}) [:item :commands]))))
  (testing "a post with no date, no summary and no tags is still one, and has no year"
    (is (= [{:id "x" :label "X" :keywords [] :value "/x/"}]
           (get-in (view/page "/" (assoc notebook :posts [{:folder "x" :title "X"}]) {:page {:kind :archive}} {})
                   [:item :commands]))))
  (testing "the pages of the author are commands too, after the posts, and say that they are pages"
    (is (= [{:id "about" :label "About" :keywords ["Who writes."] :value "/about/" :page? true}
            {:id "contact" :label "Contact" :keywords [] :value "/contact/" :page? true}]
           (drop 3 (get-in (view/page "/" with-pages {:page {:kind :archive}} {}) [:item :commands]))))))

(deftest navbar-colors-test
  (testing "the document says the colours drift behind the navbar when the settings say so"
    (is (true? (:navbar-colors? (:item (view/page "/" (assoc-in site [:blog :navbar-colors] true)
                                                  {:page {:kind :home :number 1}} {})))))
    (is (false? (:navbar-colors? (:item (view/page "/" site {:page {:kind :home :number 1}} {})))))))

(deftest page-colors-test
  (testing "the document says the colours lie behind the page when the settings say so"
    (is (true? (:page-colors? (:item (view/page "/" (assoc-in site [:blog :page-colors] true)
                                                {:page {:kind :home :number 1}} {})))))
    (is (false? (:page-colors? (:item (view/page "/" site {:page {:kind :home :number 1}} {})))))))

(defn- gradient-titles
  "Whether `site` fills the title of the post that `post` tells, of the page that `about` tells,
   and of each entry of the list that `home` tells."
  [post about home site]
  (into [(:gradient-titles? (:item (second (shown site post))))
         (:gradient-titles? (:item (second (shown site about))))]
        (map :gradient-titles?)
        (row-items (second (shown site home)) :entries)))

(deftest gradient-titles-test
  (let [filled (assoc-in with-pages [:blog :gradient-titles] true)
        post   {:page {:kind :post :folder "c"} :text {:folder "c" :title "Third" :blocks []}}
        about  {:page {:kind :standalone :folder "about"} :text {:folder "about" :title "About" :blocks []}}
        home   {:page {:kind :home :number 1}}
        titles (partial gradient-titles post about home)]
    (testing "the titles of a post, of a page and of the entries of a list are filled when the settings say so"
      (is (every? true? (titles filled))))
    (testing "and are not filled when they say nothing"
      (is (every? false? (titles with-pages))))))

(deftest standalone-test
  (let [about {:page {:kind :standalone :folder "about"} :text {:folder "about" :title "About" :blocks [one two]}}
        page  (view/page "/" with-pages about {})
        [k node] (let [{:keys [order by-key]} (get-in page [:lists :main])] [(first order) (by-key (first order))])]
    (testing "a page of the author shows its title and its text, and no date, tags or other post"
      (is (= "standalone" k))
      (is (= {:title "About" :unpublished? false :gradient-titles? false} (:item node)))
      (is (= [:blocks] (keys (:lists node))))
      (is (= [one two] (block-list node))))
    (testing "a page that is not published says so"
      (is (true? (get-in (view/page "/" with-pages {:page {:kind :standalone :folder "contact"}
                                                    :text {:folder "contact" :title "Contact" :blocks []}} {})
                         [:lists :main :by-key "standalone" :item :unpublished?]))))
    (testing "the document lists the pages in the order they were given, each with its address"
      (is (= [{:title "About" :href "/about/" :current? true} {:title "Contact" :href "/contact/" :current? false}]
             (row-items page :pages))))
    (testing "no link is to the page that is shown when that is no page of the author, also for a post of that name"
      (is (not-any? :current? (row-items (view/page "/" with-pages {:page {:kind :archive}} {}) :pages)))
      (is (not-any? :current? (row-items (view/page "/" with-pages {:page {:kind :post :folder "about"}
                                                                    :text {:folder "about" :title "A" :blocks []}} {})
                                         :pages))))
    (testing "the addresses of the pages start at the top of the blog"
      (is (= ["/my-blog/about/" "/my-blog/contact/"]
             (mapv :href (row-items (view/page "/my-blog/" with-pages about {}) :pages)))))
    (testing "a blog with no pages lists none, and a page that was told no text is not found"
      (is (= [] (row-items (view/page "/" notebook {:page {:kind :archive}} {}) :pages)))
      (is (= ["not-found"] (get-in (view/page "/" with-pages {:page {:kind :standalone :folder "gone"}} {})
                                   [:lists :main :order]))))
    (testing "the pages are not among the posts of a list"
      (is (= ["c" "b" "a"] (get-in (view/page "/" (assoc with-pages :blog (assoc blog :per-page 10))
                                              {:page {:kind :home :number 1}} {})
                                   [:lists :main :by-key "entries" :lists :entries :order]))))))

(deftest window-test
  (let [item (fn [local] (:item (view/page "/" notebook {:page {:kind :archive}} local)))]
    (testing "the document is given what the page knows of its keyboard, and what is open"
      (is (= {:command-key? true :menu? false :palette? false}
             (select-keys (item {:command-key? true}) [:command-key? :menu? :palette?])))
      (is (true? (:menu? (item {:menu? true}))))
      (is (true? (:palette? (item {:palette? true})))))))

(deftest root-test
  (let [main-of (fn [told] (get-in (view/page "/my-blog/" site told {}) [:lists :main :by-key]))
        home    (get (main-of {:page {:kind :home :number 1}}) "entries")
        page    (view/page "/my-blog/"
                           (assoc site :blog (assoc blog :template :notebook))
                           {:page {:kind :post :folder "b"} :text {:folder "b" :title "Second" :tags ["travel"]}}
                           {:tag "travel"})]
    (testing "every address of a page starts at the top of the blog"
      (is (= {:home "/my-blog/" :archive "/my-blog/archive/"} (select-keys (:item page) [:home :archive])))
      (is (= "/my-blog/c/" (:href (first (row-items home :entries)))))
      (is (= "/my-blog/tags/travel/"
             (:href (first (row-items (get-in home [:lists :entries :by-key "c"]) :tags)))))
      (is (= ["/my-blog/c/?tag=travel" "/my-blog/b/?tag=travel"] (mapv :href (row-items page :posts))))
      (is (= "/my-blog/c/" (:value (first (get-in page [:item :commands])))))
      (is (= "/my-blog/a/" (get-in page [:lists :main :by-key "post" :item :older :href])))
      (is (= {:home "/my-blog/"} (:item (get (main-of {:page {:kind :not-found}}) "not-found")))))
    (testing "a site says nothing of where its top is, and nothing of the address of a post"
      (is (not-any? :href (:posts site)))
      (is (not (contains? site :root))))))

