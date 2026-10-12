(ns bareblog.plan-test
  (:require [bareblog.plan :as plan]
            [clojure.test :refer [deftest is testing]]))

(def published
  {:blog  {:title "Bare notes"}
   :posts [{:folder "b" :title "Second" :date "2026-02-01"}
           {:folder "a" :title "First" :date "2025-01-01"}]
   :pages [{:folder "about" :title "About" :date "2026-01-15"}]})

(def today "2026-10-12")

(deftest kind-test
  (testing "a folder among the posts that is in the list is an edit"
    (is (= :edit (plan/kind {:listed-post? true :in-posts? true})))
    (is (= :edit (plan/kind {:listed-post? true :in-posts? true :in-drafts? true}))))
  (testing "a folder that is not in the list is a new post, from the posts or from the drafts"
    (is (= :new (plan/kind {:in-posts? true})))
    (is (= :new (plan/kind {:in-drafts? true}))))
  (testing "a listed post with no folder among the posts is removed, also when it is a draft again"
    (is (= :remove (plan/kind {:listed-post? true})))
    (is (= :remove (plan/kind {:listed-post? true :in-drafts? true}))))
  (testing "a page is edited, new or removed by whether it is among the pages and in the list"
    (is (= :edit (plan/kind {:listed-page? true :in-pages? true})))
    (is (= :new (plan/kind {:in-pages? true})))
    (is (= :remove (plan/kind {:listed-page? true}))))
  (testing "a name that is nowhere is unknown"
    (is (= :unknown (plan/kind {})))))

(deftest page-test
  (testing "a folder among the pages is a page, whatever the list says"
    (is (plan/page? {:in-pages? true}))
    (is (plan/page? {:in-pages? true :listed-post? true})))
  (testing "a folder among the posts or the drafts is a post, also when there is a page of that name"
    (is (not (plan/page? {:in-posts? true})))
    (is (not (plan/page? {:in-pages? true :in-posts? true})))
    (is (not (plan/page? {:in-pages? true :in-drafts? true})))
    (is (not (plan/page? {:in-posts? true :listed-page? true}))))
  (testing "a folder that is gone is what the list has it as"
    (is (plan/page? {:listed-page? true}))
    (is (not (plan/page? {:listed-post? true})))))

(deftest one-test
  (testing "an edit reads its folder, takes the others as published, and keeps its date"
    (is (= {:read         #{"b"}
            :as-published #{"a" "about"}
            :dates        {"b" "2026-02-01" "a" "2025-01-01" "about" "2026-01-15"}
            :paths        ["posts/b" "docs"]
            :message      "Publish b"}
           (plan/one published "b" {:in-posts? true} today))))
  (testing "a new post from the drafts is moved, and gets today as its date"
    (let [planned (plan/one published "c" {:in-drafts? true} today)]
      (is (= {:move "c" :read #{"c"} :as-published #{"a" "b" "about"} :message "Publish c"}
             (select-keys planned [:move :read :as-published :message])))
      (is (= today (get-in planned [:dates "c"])))))
  (testing "a new post that is among the posts already is not moved"
    (is (not (contains? (plan/one published "c" {:in-posts? true} today) :move))))
  (testing "a removal reads nothing, names what is removed, and commits the folder that went"
    (is (= {:removed #{"a"} :read #{} :as-published #{"b" "about"} :paths ["posts/a" "docs"] :message "Remove a"}
           (select-keys (plan/one published "a" {:in-drafts? true} today)
                        [:removed :read :as-published :paths :message]))))
  (testing "a name that is nowhere is refused, and nothing else is planned"
    (is (= [:refuse] (keys (plan/one published "zebra" {} today))))
    (is (re-find #"nothing named zebra" (:refuse (plan/one published "zebra" {} today)))))
  (testing "a page is read from the pages, is not moved, and gets today as the day it is first published"
    (is (= {:read #{"contact"} :as-published #{"a" "b" "about"} :paths ["pages/contact" "docs"]
            :message "Publish contact"
            :dates {"contact" today "b" "2026-02-01" "a" "2025-01-01" "about" "2026-01-15"}}
           (plan/one published "contact" {:in-pages? true} today)))
    (is (= ["pages/about" "docs"] (:paths (plan/one published "about" {:in-pages? true} today)))))
  (testing "a published page whose folder is now among the posts is published as a post, keeps
            the day it was first published, and the place it left is committed too"
    (is (= {:read #{"about"} :paths ["posts/about" "pages/about" "docs"] :message "Publish about"
            :dates {"about" "2026-01-15" "b" "2026-02-01" "a" "2025-01-01"}}
           (select-keys (plan/one published "about" {:in-posts? true} today) [:read :paths :message :dates])))
    (is (not (contains? (plan/one published "about" {:in-posts? true} today) :removed))))
  (testing "a published post whose folder is now among the pages is published as a page"
    (is (= ["pages/a" "posts/a" "docs"] (:paths (plan/one published "a" {:in-pages? true} today)))))
  (testing "a published page with no folder among the pages is removed, from the pages"
    (is (= {:removed #{"about"} :read #{} :paths ["pages/about" "docs"] :message "Remove about"}
           (select-keys (plan/one published "about" {} today) [:removed :read :paths :message]))))
  (testing "what is no name of a post folder is refused, wherever it leads"
    (doseq [folder ["../docs" "a/b" "Bad Name" ".git" "tags" ""]]
      (is (= [:refuse] (keys (plan/one published folder {:in-posts? true :in-drafts? true} today))) folder))
    (is (re-find #"no name of a folder" (:refuse (plan/one published "../docs" {:in-posts? true} today)))))
  (testing "the first post of a blog that was never published is a new one"
    (is (= {:read #{"a"} :as-published #{} :dates {"a" today}}
           (select-keys (plan/one nil "a" {:in-posts? true} today) [:read :as-published :dates])))))

(deftest all-test
  (testing "the whole blog reads every folder there is, and commits the settings too"
    (is (= {:read         #{"a" "b" "c" "about"}
            :as-published #{}
            :dates        {"a" "2025-01-01" "b" "2026-02-01" "c" today "about" "2026-01-15"}
            :paths        ["blog.edn" "docs" "posts/a" "pages/about" "posts/b" "posts/c"]
            :message      "Publish the whole blog"}
           (plan/all published {:posts #{"a" "b" "c"} :pages #{"about"}} #{} today))))
  (testing "an edit that is held back is taken as published, and its folder is not committed"
    (is (= {:read #{"a"} :as-published #{"b" "about"} :paths ["blog.edn" "docs" "posts/a"]}
           (select-keys (plan/all published {:posts #{"a" "b"} :pages #{"about"}} #{"b" "about"} today)
                        [:read :as-published :paths]))))
  (testing "what was published and has no folder any more is removed, a post or a page"
    (is (= {:removed #{"a" "about"} :read #{"b"} :paths ["blog.edn" "docs" "posts/a" "pages/about" "posts/b"]}
           (select-keys (plan/all published {:posts #{"b"} :pages #{}} #{} today) [:removed :read :paths]))))
  (testing "nothing is removed when everything that was published has its folder"
    (is (not (contains? (plan/all published {:posts #{"a" "b"} :pages #{"about"}} #{} today) :removed))))
  (testing "a new page is read, committed from the pages, and gets today as the day it is first published"
    (let [planned (plan/all published {:posts #{"a" "b"} :pages #{"about" "contact"}} #{} today)]
      (is (contains? (:read planned) "contact"))
      (is (some #{"pages/contact"} (:paths planned)))
      (is (= today (get-in planned [:dates "contact"])))))
  (testing "a published page whose folder is now among the posts is read as a post, and both
            places are committed"
    (let [planned (plan/all published {:posts #{"a" "b" "about"} :pages #{}} #{} today)]
      (is (= ["blog.edn" "docs" "posts/a" "posts/about" "pages/about" "posts/b"] (:paths planned)))
      (is (= "2026-01-15" (get-in planned [:dates "about"])))
      (is (not (contains? planned :removed)))))
  (testing "a blog that was never published reads every folder, each dated today"
    (is (= {"a" today "b" today "about" today}
           (:dates (plan/all nil {:posts #{"a" "b"} :pages #{"about"}} #{} today))))))

(deftest questions-test
  (testing "each question names the posts it is about, in the order of the alphabet"
    (is (= "Remove from the published blog: a, b?" (plan/removal-question #{"b" "a"})))
    (is (re-find #"edits that are not published: a, b\." (plan/edits-question #{"b" "a"})))))
