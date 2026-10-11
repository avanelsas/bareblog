(ns bareblog.app.view-test
  (:require [bareblog.app.view :as view]
            [clojure.test :refer [deftest is testing]]))

(def blog {:title "Bare notes" :template :journal :presets ["ocean" "forest"]})

(def one {:kind :text :content ["One."]})
(def two {:kind :text :content ["Two."]})

(def post
  {:title  "A week in Lisbon"
   :date   "2026-10-11"
   :tags   ["travel" "portugal"]
   :blocks [one two]})

(deftest post-page-test
  (let [page (view/post-page blog post)]
    (testing "the page names the blog template of the blog"
      (is (= :journal (:template page))))
    (testing "the title of the document names the post and the blog"
      (is (= "A week in Lisbon · Bare notes" (:title page))))
    (testing "the item of the page is data, with nothing about where it goes"
      (is (= {:preset "ocean" :blog-title "Bare notes" :title "A week in Lisbon" :date "2026-10-11"}
             (:item page))))
    (testing "the tags stay a list"
      (is (= ["travel" "portugal"] (:tags page))))
    (testing "the blocks are in the order of the post, each under its key"
      (is (= [one two] (mapv (:by-key page) (:order page)))))))

(deftest keys-test
  (let [order-of (fn [blocks] (:order (view/post-page blog (assoc post :blocks blocks))))]
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

(deftest missing-fields-test
  (testing "a blog that names no blog template gets the default"
    (is (= :journal (:template (view/post-page (dissoc blog :template) post)))))
  (testing "a blog with no presets asks for no preset"
    (is (nil? (get-in (view/post-page (dissoc blog :presets) post) [:item :preset]))))
  (testing "a post with no date and no tags has neither"
    (let [page (view/post-page blog {:title "Bare" :blocks []})]
      (is (nil? (get-in page [:item :date])))
      (is (= [] (:tags page)))
      (is (= [] (:order page)))))
  (testing "a tag that is named twice shows once"
    (is (= ["travel"] (:tags (view/post-page blog (assoc post :tags ["travel" "travel"])))))))
