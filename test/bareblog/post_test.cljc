(ns bareblog.post-test
  (:require [bareblog.post :as post]
            [clojure.test :refer [deftest is testing]]))

(def blocks [{:kind :text :content ["Text."]}])

(deftest from-test
  (testing "the title of the header block is the title"
    (is (= "From the header"
           (:title (post/from "a-post" {:fields {:title "From the header"} :heading "Other" :blocks blocks})))))
  (testing "with no title in the header block, the heading that opens the text is the title"
    (is (= "From the heading" (:title (post/from "a-post" {:fields {} :heading "From the heading" :blocks blocks})))))
  (testing "with neither, the name of the folder is the title"
    (is (= "a-post" (:title (post/from "a-post" {:fields {} :heading nil :blocks blocks})))))
  (testing "a post is its fields, its title and its blocks"
    (is (= {:date "2026-10-11" :tags ["travel"] :title "a-post" :blocks blocks}
           (post/from "a-post" {:fields {:date "2026-10-11" :tags ["travel"]} :heading nil :blocks blocks})))))

(deftest folder-pattern-test
  (testing "the name of a folder is small letters, digits and dashes, and does not start with a dash"
    (is (re-matches (re-pattern post/folder-pattern) "a-week-in-lisbon"))
    (is (re-matches (re-pattern post/folder-pattern) "2026-in-review"))
    (is (nil? (re-matches (re-pattern post/folder-pattern) "-draft")))
    (is (nil? (re-matches (re-pattern post/folder-pattern) "A Week")))
    (is (nil? (re-matches (re-pattern post/folder-pattern) "a/b")))))
