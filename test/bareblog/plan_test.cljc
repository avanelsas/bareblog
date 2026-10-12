(ns bareblog.plan-test
  (:require [bareblog.plan :as plan]
            [clojure.test :refer [deftest is testing]]))

(def published
  {:blog  {:title "Bare notes"}
   :posts [{:folder "b" :title "Second" :date "2026-02-01"}
           {:folder "a" :title "First" :date "2025-01-01"}]})

(def today "2026-10-12")

(deftest kind-test
  (testing "a folder among the posts that is in the list is an edit"
    (is (= :edit (plan/kind {:listed? true :in-posts? true})))
    (is (= :edit (plan/kind {:listed? true :in-posts? true :in-drafts? true}))))
  (testing "a folder that is not in the list is a new post, from the posts or from the drafts"
    (is (= :new (plan/kind {:in-posts? true})))
    (is (= :new (plan/kind {:in-drafts? true}))))
  (testing "a listed post with no folder among the posts is removed, also when it is a draft again"
    (is (= :remove (plan/kind {:listed? true})))
    (is (= :remove (plan/kind {:listed? true :in-drafts? true}))))
  (testing "a name that is nowhere is unknown"
    (is (= :unknown (plan/kind {})))))

(deftest one-test
  (testing "an edit reads its folder, takes the others as published, and keeps its date"
    (is (= {:read         #{"b"}
            :as-published #{"a"}
            :dates        {"b" "2026-02-01" "a" "2025-01-01"}
            :paths        ["posts/b" "docs"]
            :message      "Publish b"}
           (plan/one published "b" {:in-posts? true} today))))
  (testing "a new post from the drafts is moved, and gets today as its date"
    (let [planned (plan/one published "c" {:in-drafts? true} today)]
      (is (= {:move "c" :read #{"c"} :as-published #{"a" "b"} :message "Publish c"}
             (select-keys planned [:move :read :as-published :message])))
      (is (= today (get-in planned [:dates "c"])))))
  (testing "a new post that is among the posts already is not moved"
    (is (not (contains? (plan/one published "c" {:in-posts? true} today) :move))))
  (testing "a removal reads nothing, names what is removed, and commits the folder that went"
    (is (= {:removed #{"a"} :read #{} :as-published #{"b"} :paths ["posts/a" "docs"] :message "Remove a"}
           (select-keys (plan/one published "a" {:in-drafts? true} today)
                        [:removed :read :as-published :paths :message]))))
  (testing "a name that is nowhere is refused, and nothing else is planned"
    (is (= [:refuse] (keys (plan/one published "zebra" {} today))))
    (is (re-find #"no post zebra" (:refuse (plan/one published "zebra" {} today)))))
  (testing "what is no name of a post folder is refused, wherever it leads"
    (doseq [folder ["../docs" "a/b" "Bad Name" ".git" "tags" ""]]
      (is (= [:refuse] (keys (plan/one published folder {:in-posts? true :in-drafts? true} today))) folder))
    (is (re-find #"no name of a post folder" (:refuse (plan/one published "../docs" {:in-posts? true} today)))))
  (testing "the first post of a blog that was never published is a new one"
    (is (= {:read #{"a"} :as-published #{} :dates {"a" today}}
           (select-keys (plan/one nil "a" {:in-posts? true} today) [:read :as-published :dates])))))

(deftest all-test
  (testing "the whole blog reads every folder there is, and commits the settings too"
    (is (= {:read         #{"a" "b" "c"}
            :as-published #{}
            :dates        {"a" "2025-01-01" "b" "2026-02-01" "c" today}
            :paths        ["blog.edn" "docs" "posts/a" "posts/b" "posts/c"]
            :message      "Publish the whole blog"}
           (plan/all published #{"a" "b" "c"} #{} today))))
  (testing "an edit that is held back is taken as published, and its folder is not committed"
    (is (= {:read #{"a"} :as-published #{"b"} :paths ["blog.edn" "docs" "posts/a"]}
           (select-keys (plan/all published #{"a" "b"} #{"b"} today) [:read :as-published :paths]))))
  (testing "a published post with no folder any more is removed, and its folder is committed"
    (is (= {:removed #{"a"} :read #{"b"} :paths ["blog.edn" "docs" "posts/a" "posts/b"]}
           (select-keys (plan/all published #{"b"} #{} today) [:removed :read :paths]))))
  (testing "nothing is removed when every published post has its folder"
    (is (not (contains? (plan/all published #{"a" "b"} #{} today) :removed))))
  (testing "a blog that was never published reads every folder, each dated today"
    (is (= {"a" today "b" today} (:dates (plan/all nil #{"a" "b"} #{} today))))))

(deftest questions-test
  (testing "each question names the posts it is about, in the order of the alphabet"
    (is (= "Remove from the published blog: a, b?" (plan/removal-question #{"b" "a"})))
    (is (re-find #"edits that are not published: a, b\." (plan/edits-question #{"b" "a"})))))
