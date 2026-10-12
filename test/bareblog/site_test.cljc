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

(deftest newest-folder-test
  (testing "the newest post is the first in the list"
    (is (= "b" (site/newest-folder {:posts [{:folder "b"} {:folder "a"}]}))))
  (testing "a site with no posts, and one that could not be read, have none"
    (is (nil? (site/newest-folder {:posts []})))
    (is (nil? (site/newest-folder {:problem "No settings."})))))

(deftest page-count-test
  (let [of (fn [n per-page] (site/page-count {:blog {:per-page per-page} :posts (repeat n {})}))]
    (testing "the posts fill as many pages as it takes, and the last may be short"
      (is (= [1 1 2 2 3] (mapv #(of % 2) [1 2 3 4 5]))))
    (testing "no posts fill one page"
      (is (= 1 (of 0 2))))
    (testing "a blog that asks for nothing gets ten on a page"
      (is (= 10 (site/per-page {:blog {}})))
      (is (= 2 (site/page-count {:blog {} :posts (repeat 11 {})}))))))

(deftest tags-test
  (testing "the tags of a site are every tag of its posts, once, in the order of the alphabet"
    (is (= ["food" "travel" "web"]
           (site/tags {:posts [{:tags ["travel" "web"]} {:tags ["food" "travel"]} {}]})))))

(deftest problems-test
  (testing "what is wrong is added after what was wrong already"
    (is (= ["First." "Second."] (:problems (site/with-problems {:problems ["First."]} ["Second."]))))
    (is (= ["First."] (:problems (site/with-problems {} ["First."])))))
  (testing "a site with nothing wrong says nothing of problems"
    (is (= {:blog {}} (site/with-problems {:blog {}} []))))
  (testing "the readers of a published blog are told nothing of what is wrong"
    (is (= {:blog {} :posts []} (site/published {:blog {} :posts [] :problems ["First."]})))))

(deftest pages-test
  (testing "the entry of a page says enough to link to it, and nothing of a date or tags"
    (is (= {:folder "about" :title "About" :order 1 :summary "Who."}
           (site/page-entry "about" {:title "About" :order 1 :summary "Who." :date "2026-01-01" :tags ["x"]
                                     :blocks []}))))
  (testing "the pages are in the order of their numbers, and those with none follow by title"
    (is (= ["projects" "about" "contact" "colophon"]
           (map :folder (:pages (site/with-pages {} [{:folder "contact" :title "Contact"}
                                                     {:folder "about" :title "About" :order 2}
                                                     {:folder "colophon" :title "Zed"}
                                                     {:folder "projects" :title "Projects" :order 1}]))))))
  (testing "a site with no pages says nothing of them"
    (is (= {:blog {}} (site/with-pages {:blog {}} [])))))

