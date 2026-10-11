(ns bareblog.app.templates-test
  (:require [bareblog.app.blocks-test :refer [all-fixed? examples]]
            [bareblog.app.templates :as templates]
            [baremirror.alpha.template :as template]
            [clojure.test :refer [deftest is testing]]))

(defn- part-names
  "The part names in the fixed template `fixed`."
  [fixed]
  (set (keep :data-x-part (filter map? (tree-seq coll? seq fixed)))))

(deftest named-test
  (testing "the default blog template has a name"
    (is (some? (templates/named templates/default))))
  (testing "a name with no blog template is refused, with the name"
    (is (= {:template :poster}
           (ex-data (try (templates/named :poster) (catch #?(:clj Exception :cljs :default) e e)))))))

(deftest every-blog-template-test
  (doseq [[template-name {:keys [key page tag tags blocks block]}] templates/by-name]
    (testing (str template-name " has a key that is text")
      (is (string? key)))
    (testing (str template-name " has a page and a tag that are split templates")
      (is (template/split? page))
      (is (template/split? tag)))
    (testing (str template-name " has a page that is fixed all the way down once it is split")
      (is (all-fixed? (:fixed page))))
    (testing (str template-name " names the parts that hold the tags and the blocks")
      (is (contains? (part-names (:fixed page)) tags))
      (is (contains? (part-names (:fixed page)) blocks)))
    (testing (str template-name " has a template for a block")
      (is (= [:p "Text."] (block {:kind :text :content ["Text."]}))))))

(deftest journal-holes-test
  (let [{:keys [page tag]} (templates/named :journal)
        item               {:preset "ocean" :blog-title "Bare notes" :title "A week in Lisbon"
                            :date "2026-10-11"}]
    (testing "the page takes its preset, the name of the blog, and the title and date of the post"
      (is (= #{{:attrs {:preset "ocean"}}
               {:attrs {:label "Bare notes"}}
               {:text "Bare notes"}
               {:text "A week in Lisbon"}
               {:text "2026-10-11"}}
             (set (vals (template/writes page item))))))
    (testing "a tag takes its text"
      (is (= [{:text "travel"}] (vals (template/writes tag "travel")))))))

(deftest custom-elements-test
  (let [journal (templates/named :journal)]
    (testing "a page with no blocks has the custom elements of its blog template"
      (is (= ["x-container" "x-navbar" "x-theme"] (vec (templates/custom-elements journal [])))))
    (testing "a page has the custom elements of its blocks too, at any depth"
      (is (= ["x-code" "x-container" "x-image" "x-navbar" "x-table" "x-table-cell" "x-table-row" "x-theme"]
             (vec (templates/custom-elements
                   journal
                   [(:photo examples)
                    (:table examples)
                    {:kind :quote :blocks [(:code examples)]}])))))))
