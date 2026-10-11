(ns bareblog.app.templates-test
  (:require [bareblog.app.blocks-test :refer [all-fixed? examples]]
            [bareblog.app.templates :as templates]
            [bareblog.app.view :as view]
            [baremirror.alpha.template :as template]
            [clojure.test :refer [deftest is testing]]))

(def site
  {:blog  {:title "Bare notes" :template :journal :presets ["ocean"]}
   :posts [{:folder "b" :title "Second" :date "2026-02-01" :tags ["travel"] :summary "Two." :draft? true}
           {:folder "a" :title "First" :date "2025-01-01" :summary "One."}]})

(def pages
  "A page of each kind, as a page is told it."
  {:home      {:page {:kind :home :number 1}}
   :tag       {:page {:kind :tag :tag "travel"}}
   :archive   {:page {:kind :archive}}
   :post      {:page {:kind :post :folder "a"}
               :post {:title "First" :tags ["travel"] :blocks (vec (vals examples))}}
   :not-found {:page {:kind :not-found}}})

(defn- part-names
  "The part names in the fixed template `fixed`."
  [fixed]
  (set (keep :data-x-part (filter map? (tree-seq coll? seq fixed)))))

(defn- nodes
  "The node `node` of a blog template and every node below it."
  [{:keys [lists] :as node}]
  (cons node (mapcat nodes (mapcat (fn [{:keys [row rows]}] (if row [row] (vals rows))) (vals lists)))))

(deftest named-test
  (testing "the default blog template has a name"
    (is (some? (templates/named templates/default))))
  (testing "a name with no blog template is refused, with the name"
    (is (= {:template :poster}
           (ex-data (try (templates/named :poster) (catch #?(:clj Exception :cljs :default) e e)))))))

(deftest every-blog-template-test
  (doseq [[template-name blog-template] templates/by-name]
    (testing (str template-name " has a key that is text")
      (is (string? (:key blog-template))))
    (doseq [{:keys [split fixed-of lists]} (nodes blog-template)]
      (testing (str template-name ": a node has a split template or a function that gives a fixed one")
        (is (or (template/split? split) (fn? fixed-of))))
      (testing (str template-name ": a split template is fixed all the way down once it is split")
        (is (or (nil? split) (all-fixed? (:fixed split)))))
      (testing (str template-name ": each list names a part that its node has")
        (is (every? (part-names (:fixed split)) (map :part (vals lists))))))
    (testing (str template-name " shows every kind of page with templates that are fixed all the way down")
      (doseq [[kind told] pages]
        (is (every? all-fixed? (templates/fixed-templates blog-template (view/page site told))) kind)))))

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
              :let                 [{:keys [order by-key]} (get-in shown [:lists list-name])]
              k                    order
              pair                 (shown-nodes (templates/row-node row-list k) (by-key k))]
          pair)))

(defn- keyword-holes
  "The holes of `split` that are a key of the item."
  [split]
  (filter keyword? (mapcat (fn [{:keys [attrs text]}] (cons text (vals attrs))) (:holes split))))

(deftest view-and-template-test
  (let [journal (templates/named :journal)]
    (testing "the view of every kind of page names a page that Journal has a node for"
      (doseq [[kind told] pages]
        (is (some? (templates/row-node (get-in journal [:lists :main])
                                       (first (get-in (view/page site told) [:lists :main :order]))))
            kind)))
    (testing "the view gives every node the key of each hole that is a key, for every kind of page"
      (doseq [[kind told]            pages
              [{:keys [split]} shown] (shown-nodes journal (view/page site told))
              hole                   (keyword-holes split)]
        (is (contains? (:item shown) hole) (str kind " " hole))))
    (testing "every hole of every node can be filled from what the view gives, for every kind of page"
      (doseq [[kind told]            pages
              [{:keys [split]} shown] (shown-nodes journal (view/page site told))
              :when                  split]
        (is (map? (template/writes split (:item shown))) (str kind))))))

(deftest words-test
  (testing "Journal names each kind of page in the title of the document"
    (let [title (:title (templates/named :journal))]
      (is (nil? (title {:kind :home :number 1})))
      (is (= "Page 2" (title {:kind :home :number 2})))
      (is (= "travel" (title {:kind :tag :tag "travel"})))
      (is (= "Archive" (title {:kind :archive})))
      (is (= "A week in Lisbon" (title {:kind :post :post "A week in Lisbon"})))
      (is (= "Not found" (title {:kind :not-found}))))))

(defn- hidden-count
  "How many parts of the node of the page hide themselves, when the page `told` shows `site`."
  [site told]
  (let [journal          (templates/named :journal)
        page             (view/page site told)
        k                (first (get-in page [:lists :main :order]))
        {:keys [split]}  (templates/row-node (get-in journal [:lists :main]) k)
        shown            (get-in page [:lists :main :by-key k])]
    (count (filter (comp true? :hidden :attrs) (vals (template/writes split (:item shown)))))))

(deftest hidden-test
  (let [empty-site (assoc site :posts [])
        two-pages  (assoc-in site [:blog :per-page] 1)]
    (testing "a home page on one page hides its heading, its page numbers and the text for no posts"
      (is (= 3 (hidden-count site (:home pages)))))
    (testing "a home page of more pages shows its page numbers"
      (is (= 2 (hidden-count two-pages (:home pages)))))
    (testing "the page of a tag shows its heading"
      (is (= 2 (hidden-count site (:tag pages)))))
    (testing "a home page with no posts shows the text for that"
      (is (= 2 (hidden-count empty-site (:home pages)))))
    (testing "the archive hides the text for no posts, and shows it when there are none"
      (is (= 1 (hidden-count site (:archive pages))))
      (is (= 0 (hidden-count empty-site (:archive pages)))))
    (testing "the page of a draft with a newer and an older post hides nothing"
      (is (= 0 (hidden-count (update site :posts (partial into [{:folder "c" :title "Third" :date "2027-01-01"}]))
                             {:page {:kind :post :folder "b"} :post {:title "Second" :blocks []}}))))
    (testing "the page of the newest post, a draft, hides the link to a newer one"
      (is (= 1 (hidden-count site {:page {:kind :post :folder "b"} :post {:title "Second" :blocks []}}))))
    (testing "the page of a published post hides the badge of a draft, and a link with no post"
      (is (= 2 (hidden-count site {:page {:kind :post :folder "a"} :post {:title "First" :blocks []}}))))))

(deftest custom-elements-test
  (let [journal (templates/named :journal)
        tags-of (fn [told] (vec (templates/custom-elements journal (view/page site told))))]
    (testing "a page has the custom elements of the document and of what it shows"
      (is (= ["x-container" "x-navbar" "x-theme"] (tags-of (:not-found pages))))
      (is (= ["x-badge" "x-container" "x-navbar" "x-pagination" "x-theme"] (tags-of (:home pages))))
      (is (= ["x-badge" "x-container" "x-navbar" "x-theme"] (tags-of (:archive pages)))))
    (testing "the page of a post has the custom elements of its blocks too, at any depth"
      (is (= ["x-badge" "x-code" "x-container" "x-image" "x-navbar" "x-table" "x-table-cell"
              "x-table-row" "x-theme"]
             (tags-of (:post pages)))))))

(deftest events-test
  (testing "the events of Journal are the ones its templates name, by event type and part"
    (is (= [:to-page] (vals (templates/events (templates/named :journal)))))
    (is (= ["page-change-request"] (map first (keys (templates/events (templates/named :journal))))))))
