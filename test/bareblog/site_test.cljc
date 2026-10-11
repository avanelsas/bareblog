(ns bareblog.site-test
  (:require [bareblog.site :as site]
            [clojure.test :refer [deftest is testing]]))

(def post
  {:title   "A week in Lisbon"
   :date    "2026-10-11"
   :tags    ["travel"]
   :summary "Seven days."
   :blocks  [{:kind :text :content ["Text."]}]})

(deftest entry-test
  (testing "an entry is what a list shows of a post, and the folder to link to"
    (is (= {:folder "a-week-in-lisbon" :title "A week in Lisbon" :date "2026-10-11"
            :tags ["travel"] :summary "Seven days."}
           (site/entry "a-week-in-lisbon" post false))))
  (testing "an entry holds no blocks"
    (is (not (contains? (site/entry "a-week-in-lisbon" post false) :blocks))))
  (testing "a draft says that it is one"
    (is (true? (:draft? (site/entry "a-new-idea" post true)))))
  (testing "a post with no date and no tags has neither"
    (is (= {:folder "bare" :title "Bare"} (site/entry "bare" {:title "Bare" :blocks []} false)))))

(defn- folders [entries]
  (mapv :folder (site/listed entries)))

(deftest listed-test
  (testing "the latest date comes first"
    (is (= ["new" "old"] (folders [{:folder "old" :title "Old" :date "2025-01-01"}
                                  {:folder "new" :title "New" :date "2026-01-01"}]))))
  (testing "a post with no date comes after the dated ones"
    (is (= ["dated" "bare"] (folders [{:folder "bare" :title "A"}
                                      {:folder "dated" :title "Z" :date "2020-01-01"}]))))
  (testing "posts of one day, and posts with no date, are in the order of their titles"
    (is (= ["a" "b" "c" "d"] (folders [{:folder "b" :title "B" :date "2026-01-01"}
                                       {:folder "d" :title "D"}
                                       {:folder "a" :title "A" :date "2026-01-01"}
                                       {:folder "c" :title "C"}])))))

(deftest site-test
  (testing "the site is the settings and the list"
    (is (= {:blog {:title "Bare notes"} :posts [{:folder "a" :title "A"}]}
           (site/site {:title "Bare notes"} [{:folder "a" :title "A"}] []))))
  (testing "what is wrong with the blog is in the site, when something is"
    (is (= ["posts/tags is left out."]
           (:problems (site/site {:title "Bare notes"} [] ["posts/tags is left out."]))))))

(deftest told-test
  (let [site {:blog {:title "Bare notes"} :posts []}
        page {:page {:kind :archive}}]
    (testing "a page is told the site and its own data"
      (is (= {:site site :page page} (site/told site page))))
    (testing "a problem in place of the site is what the page is told"
      (is (= {:problem "No settings."} (site/told {:problem "No settings."} page))))
    (testing "a problem in place of the data of the page is what the page is told"
      (is (= {:problem "No post."} (site/told site {:problem "No post."}))))))

(deftest notice-test
  (testing "a site with nothing wrong has no notice"
    (is (nil? (site/notice {:blog {} :posts []}))))
  (testing "one problem is the notice"
    (is (= "A is left out." (site/notice {:problems ["A is left out."]}))))
  (testing "the first of more problems is the notice, with how many more there are"
    (is (= "A is left out. There are 2 more in the terminal."
           (site/notice {:problems ["A is left out." "B." "C."]})))))
