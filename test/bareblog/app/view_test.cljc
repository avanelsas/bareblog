(ns bareblog.app.view-test
  (:require [bareblog.app.view :as view]
            [clojure.test :refer [deftest is testing]]))

(def blog {:title "Bare notes" :template :journal :presets ["ocean" "forest"] :per-page 2})

(def posts
  [{:folder "c" :title "Third" :date "2026-03-01" :tags ["travel" "food"] :summary "Three."}
   {:folder "b" :title "Second" :date "2026-02-01" :tags ["travel"] :summary "Two." :draft? true}
   {:folder "a" :title "First" :date "2025-01-01" :summary "One."}])

(def site {:blog blog :posts posts})

(defn- shown
  "The key and the node of what the page `told` shows of `site`."
  ([told] (shown site told))
  ([site told]
   (let [{:keys [order by-key]} (get-in (view/page site told) [:lists :main])]
     [(first order) (by-key (first order))])))

(defn- row-items
  "The item of each row of the list `list-name` of `node`, in order."
  [node list-name]
  (let [{:keys [order by-key]} (get-in node [:lists list-name])]
    (mapv (comp :item by-key) order)))

(deftest document-test
  (let [page (view/page site {:page {:kind :archive}})]
    (testing "the page names the blog template and the language of the blog"
      (is (= :journal (:template page)))
      (is (= "en" (:language page)))
      (is (= "nl" (:language (view/page (assoc-in site [:blog :language] "nl") {:page {:kind :archive}})))))
    (testing "the document is given the preset, the name of the blog and the addresses of its links"
      (is (= {:preset "ocean" :blog-title "Bare notes" :home "/" :archive "/archive/"} (:item page))))
    (testing "the document holds one page"
      (is (= 1 (count (get-in page [:lists :main :order])))))
    (testing "a blog that names no blog template gets the default, and no preset is none"
      (let [bare (view/page {:blog {:title "Bare"} :posts []} {:page {:kind :archive}})]
        (is (= :journal (:template bare)))
        (is (nil? (get-in bare [:item :preset])))))))

(deftest title-test
  (testing "the first page of the home page has the name of the blog"
    (is (= "Bare notes" (:title (view/page site {:page {:kind :home :number 1}})))))
  (testing "every other page names itself first"
    (is (= "Page 2 · Bare notes" (:title (view/page site {:page {:kind :home :number 2}}))))
    (is (= "travel · Bare notes" (:title (view/page site {:page {:kind :tag :tag "travel"}}))))
    (is (= "Archive · Bare notes" (:title (view/page site {:page {:kind :archive}}))))
    (is (= "Third · Bare notes"
           (:title (view/page site {:page {:kind :post :folder "c"} :post {:title "Third" :blocks []}}))))))

(deftest home-test
  (let [[k node] (shown {:page {:kind :home :number 1}})]
    (testing "the home page is a page of entries with no heading"
      (is (= "entries" k))
      (is (= {:heading nil :count 2 :number 1 :pages 2} (:item node))))
    (testing "it shows the first posts of the list, as many as the blog asks for on a page"
      (is (= ["c" "b"] (get-in node [:lists :entries :order]))))
    (testing "an entry shows the title, the date, the summary and the address of its post"
      (is (= {:title "Third" :date "2026-03-01" :href "/c/" :draft? false :summary "Three."}
             (first (row-items node :entries)))))
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

(deftest tag-test
  (let [[k node] (shown {:page {:kind :tag :tag "travel"}})]
    (testing "the page of a tag is a page of entries under the tag, on one page"
      (is (= "entries" k))
      (is (= {:heading "travel" :count 2 :number 1 :pages 1} (:item node))))
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

(def one {:kind :text :content ["One."]})
(def two {:kind :text :content ["Two."]})

(defn- post-node [folder post]
  (second (shown {:page {:kind :post :folder folder} :post post})))

(deftest post-test
  (let [node (post-node "b" {:title "Second" :date "2026-02-01" :tags ["travel" "travel"] :blocks [one two]})]
    (testing "the page of a post shows its title and its date, and that it is a draft"
      (is (= {:title "Second" :date "2026-02-01" :draft? true}
             (select-keys (:item node) [:title :date :draft?]))))
    (testing "it has the title and the address of the newer post and of the older one"
      (is (= {:newer {:title "Third" :href "/c/"} :older {:title "First" :href "/a/"}}
             (select-keys (:item node) [:newer :older]))))
    (testing "a tag that is named twice shows once"
      (is (= [{:name "travel" :href "/tags/travel/"}] (row-items node :tags))))
    (testing "the blocks are in the order of the post, each under its key"
      (is (= [one two] (mapv (get-in node [:lists :blocks :by-key]) (get-in node [:lists :blocks :order]))))))
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
    (is (= "Not found · Bare notes" (:title (view/page site {:page {:kind :not-found}}))))))
