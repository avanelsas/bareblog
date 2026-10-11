(ns bareblog.watch-test
  (:require [bareblog.watch :as watch]
            [clojure.test :refer [deftest is testing]]))

(def stamp
  {"blog.edn"                           1
   "posts/a-week-in-lisbon/post.md"     1
   "posts/a-week-in-lisbon/harbour.svg" 1
   "posts/why-web-components/post.md"   1
   "drafts/a-new-idea/post.md"          1})

(deftest changes-test
  (testing "the same stamp has no changes"
    (is (= {:folders #{} :settings? false} (watch/changes stamp stamp))))
  (testing "a file with another time changes its folder"
    (is (= {:folders #{"a-week-in-lisbon"} :settings? false}
           (watch/changes stamp (assoc stamp "posts/a-week-in-lisbon/post.md" 2)))))
  (testing "a photo beside a post changes its folder too"
    (is (= #{"a-week-in-lisbon"}
           (:folders (watch/changes stamp (assoc stamp "posts/a-week-in-lisbon/harbour.svg" 2))))))
  (testing "a draft is a folder like any other"
    (is (= #{"a-new-idea"}
           (:folders (watch/changes stamp (assoc stamp "drafts/a-new-idea/post.md" 2))))))
  (testing "a new file and a file that is gone change their folders"
    (is (= #{"a-week-in-lisbon" "why-web-components"}
           (:folders (watch/changes stamp (-> stamp
                                              (assoc "posts/a-week-in-lisbon/tram.jpg" 5)
                                              (dissoc "posts/why-web-components/post.md")))))))
  (testing "the settings file changes the settings and no folder"
    (is (= {:folders #{} :settings? true} (watch/changes stamp (assoc stamp "blog.edn" 2)))))
  (testing "a file that is in no post folder changes nothing"
    (is (= {:folders #{} :settings? false}
           (watch/changes stamp (assoc stamp "notes.txt" 2 "posts/stray.md" 2 "docs/x/index.html" 2))))))

(deftest to-tell-test
  (testing "a changed folder that a page shows is told"
    (is (= #{"a-week-in-lisbon"}
           (watch/to-tell {:folders #{"a-week-in-lisbon" "a-new-idea"} :settings? false}
                          #{"a-week-in-lisbon" "why-web-components"}))))
  (testing "a changed folder that no page shows is not"
    (is (= #{} (watch/to-tell {:folders #{"a-new-idea"} :settings? false} #{"a-week-in-lisbon"}))))
  (testing "a change of the settings tells every page"
    (is (= #{"a-week-in-lisbon" "why-web-components"}
           (watch/to-tell {:folders #{} :settings? true} #{"a-week-in-lisbon" "why-web-components"}))))
  (testing "no change tells nobody"
    (is (= #{} (watch/to-tell {:folders #{} :settings? false} #{"a-week-in-lisbon"}))))
  (testing "nobody listening is told nothing, whatever changed"
    (is (= #{} (watch/to-tell {:folders #{"a-week-in-lisbon"} :settings? true} #{})))))

(deftest by-folder-test
  (testing "each folder has the listeners that show it"
    (is (= {"a-week-in-lisbon" #{:first :third} "a-new-idea" #{:second}}
           (update-vals (watch/by-folder {:first  "a-week-in-lisbon"
                                          :second "a-new-idea"
                                          :third  "a-week-in-lisbon"})
                        set))))
  (testing "nobody listening is no folders"
    (is (= {} (watch/by-folder {})))))
