(ns bareblog.blog.site-test
  (:require [bareblog.blog.site :as site]
            [clojure.test :refer [deftest is testing]]))

(def post
  {:title   "A week in Lisbon"
   :date    "2026-10-11"
   :tags    ["travel"]
   :summary "Seven days."
   :blocks  [{:kind :text :content ["Text."]}]})

(deftest powered-by-footer-test
  (testing "a blog shows the line \"Powered by BareBlog\" unless its settings say false"
    (is (site/powered-by-footer? {}))
    (is (site/powered-by-footer? {:powered-by-footer true}))
    (is (not (site/powered-by-footer? {:powered-by-footer false}))))
  (testing "a value that is not true or false shows the line, and is named as a problem"
    (is (site/powered-by-footer? {:powered-by-footer "no"}))
    (is (= ["blog.edn gives \"no\" as its :powered-by-footer, which is true or false."]
           (site/settings-problems {:powered-by-footer "no"}))))
  (testing "true, false or nothing is no problem"
    (doseq [settings [{} {:powered-by-footer true} {:powered-by-footer false}]]
      (is (empty? (site/settings-problems settings))))))

(deftest gradient-titles-test
  (testing "the titles are filled only when the settings say true"
    (is (site/gradient-titles? {:gradient-titles true}))
    (is (not-any? site/gradient-titles? [{} {:gradient-titles false} {:gradient-titles "yes"}])))
  (testing "a value that is not true or false is named as a problem"
    (is (= ["blog.edn gives \"yes\" as its :gradient-titles, which is true or false."]
           (site/settings-problems {:gradient-titles "yes"}))))
  (testing "true, false or nothing is no problem"
    (doseq [settings [{} {:gradient-titles true} {:gradient-titles false}]]
      (is (empty? (site/settings-problems settings))))))

(deftest navbar-colors-test
  (testing "the colours drift behind the navbar only when the settings say true"
    (is (site/navbar-colors? {:navbar-colors true}))
    (is (not-any? site/navbar-colors? [{} {:navbar-colors false} {:navbar-colors "yes"}])))
  (testing "a value that is not true or false is named as a problem, and true or false is none"
    (is (= ["blog.edn gives \"yes\" as its :navbar-colors, which is true or false."]
           (site/settings-problems {:navbar-colors "yes"})))
    (is (empty? (site/settings-problems {:navbar-colors true})))))

(deftest page-colors-test
  (testing "the colours lie behind the page only when the settings say true"
    (is (site/page-colors? {:page-colors true}))
    (is (not-any? site/page-colors? [{} {:page-colors false} {:page-colors "yes"}])))
  (testing "a value that is not true or false is named as a problem, and true or false is none"
    (is (= ["blog.edn gives \"yes\" as its :page-colors, which is true or false."]
           (site/settings-problems {:page-colors "yes"})))
    (is (empty? (site/settings-problems {:page-colors true})))))

(deftest days-test
  (testing "the days of the posts are each day once, oldest first, and a post with no date has none"
    (is (= ["2026-10-02" "2026-10-12"]
           (site/post-days {:posts [{:date "2026-10-12T09:00:00+02:00"} {:date "2026-10-02"}
                                    {:date "2026-10-12T21:00:00+02:00"} {:title "Bare"}]})))))


(deftest entry-test
  (testing "an entry is what a list shows of a post, and the folder to link to"
    (is (= {:folder "a-week-in-lisbon" :title "A week in Lisbon" :date "2026-10-11"
            :tags ["travel"] :summary "Seven days."}
           (site/entry "a-week-in-lisbon" post false))))
  (testing "an entry holds no blocks"
    (is (not (contains? (site/entry "a-week-in-lisbon" post false) :blocks))))
  (testing "a draft says that it is one"
    (is (true? (:draft? (site/entry "a-new-idea" post true)))))
  (testing "an entry has the address of the first picture of the post"
    (is (= "harbour.jpg"
           (:picture (site/entry "a" (assoc post :blocks [{:kind :text :content ["Text."]}
                                                          {:kind :photo :src "harbour.jpg" :width 300 :height 200}
                                                          {:kind :photo :src "later.jpg"}])
                                 false)))))
  (testing "a post with no picture has none"
    (is (not (contains? (site/entry "a-week-in-lisbon" post false) :picture))))
  (testing "a post with no date and no tags has neither"
    (is (= {:folder "bare" :title "Bare"} (site/entry "bare" {:title "Bare" :blocks []} false)))))

(defn- folders [entries]
  (mapv :folder (site/listed entries)))

(deftest listed-test
  (testing "the latest date comes first"
    (is (= ["new" "old"] (folders [{:folder "old" :title "Old" :date "2025-01-01"}
                                  {:folder "new" :title "New" :date "2026-01-01"}]))))
  (testing "a post with no date, which was never published, comes before the dated ones"
    (is (= ["bare" "dated"] (folders [{:folder "dated" :title "A" :date "2020-01-01T09:00:00+01:00"}
                                      {:folder "bare" :title "Z"}]))))
  (testing "of two posts of one day, the one that was published later comes first"
    (is (= ["evening" "morning"] (folders [{:folder "morning" :title "A" :date "2026-01-01T08:00:00+01:00"}
                                           {:folder "evening" :title "Z" :date "2026-01-01T20:00:00+01:00"}]))))
  (testing "posts of one moment, and posts with no date, are in the order of their titles"
    (is (= ["c" "d" "a" "b"] (folders [{:folder "b" :title "B" :date "2026-01-01T08:00:00+01:00"}
                                       {:folder "d" :title "D"}
                                       {:folder "a" :title "A" :date "2026-01-01T08:00:00+01:00"}
                                       {:folder "c" :title "C"}])))))

(deftest day-test
  (testing "the day of a date is its first ten characters, and no date has no day"
    (is (= "2026-10-12" (site/day "2026-10-12T14:03:22+02:00")))
    (is (nil? (site/day nil)))))

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
  (testing "the entry of a page says enough to link to it, with the day it was first published"
    (is (= {:folder "about" :title "About" :date "2026-01-01" :summary "Who."}
           (site/page-entry "about" {:title "About" :summary "Who." :date "2026-01-01" :tags ["x"] :blocks []}))))
  (testing "the pages are in the order they were first published, and those of one day by title"
    (is (= ["contact" "about" "projects"]
           (map :folder (:pages (site/with-pages {} [{:folder "projects" :title "Projects" :date "2026-03-01"}
                                                     {:folder "about" :title "About" :date "2026-03-01"}
                                                     {:folder "contact" :title "Contact" :date "2026-01-01"}]))))))
  (testing "a page that was never published comes after those that were"
    (is (= ["contact" "about"]
           (map :folder (:pages (site/with-pages {} [{:folder "about" :title "About"}
                                                     {:folder "contact" :title "Contact" :date "2026-01-01"}]))))))
  (testing "the dates of a site are those of its posts and of its pages"
    (is (= {"a" "2026-01-01" "about" "2026-02-01"}
           (site/dates {:posts [{:folder "a" :date "2026-01-01"} {:folder "b"}]
                        :pages [{:folder "about" :date "2026-02-01"}]}))))
  (testing "a site with no pages says nothing of them"
    (is (= {:blog {}} (site/with-pages {:blog {}} [])))))
