(ns bareblog.watch-test
  (:require [bareblog.watch :as watch]
            [clojure.test :refer [deftest is testing]]))

(def stamp
  {"blog.edn"                       1
   "posts/a-week-in-lisbon/post.md" 1
   "drafts/a-new-idea/post.md"      1})

(deftest changed-test
  (testing "the same stamp has not changed"
    (is (not (watch/changed? stamp stamp))))
  (testing "a file with another time, a new file and a file that is gone are each a change"
    (is (watch/changed? stamp (assoc stamp "posts/a-week-in-lisbon/post.md" 2)))
    (is (watch/changed? stamp (assoc stamp "posts/a-week-in-lisbon/tram.jpg" 5)))
    (is (watch/changed? stamp (dissoc stamp "drafts/a-new-idea/post.md")))
    (is (watch/changed? stamp (assoc stamp "blog.edn" 2)))))

(deftest by-page-test
  (testing "each page has the listeners that show it"
    (is (= {{:kind :home :number 1} #{:first :third} {:kind :archive} #{:second}}
           (update-vals (watch/by-page {:first  {:kind :home :number 1}
                                        :second {:kind :archive}
                                        :third  {:kind :home :number 1}})
                        set))))
  (testing "nobody listening is no pages"
    (is (= {} (watch/by-page {})))))
