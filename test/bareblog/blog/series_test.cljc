(ns bareblog.blog.series-test
  (:require [bareblog.blog.series :as series]
            #?(:clj [clojure.edn :as edn] :cljs [cljs.reader :as edn])
            [clojure.test :refer [deftest is testing]]))

(def all
  {"Lisbon" ["start" "food" "end"]
   "Eating" ["dinner"]})

(def posts
  [{:folder "end" :title "The end"}
   {:folder "food" :title "The food"}
   {:folder "start" :title "The start"}
   {:folder "dinner" :title "Dinner"}])

(deftest well-formed-test
  (testing "the series are a map from a name to the folders of its parts"
    (is (series/well-formed? all))
    (is (series/well-formed? {})))
  (testing "anything else is not"
    (is (not-any? series/well-formed? [[] {"A" "start"} {"A" ["start" 1]} {:a ["start"]} nil]))))

(deftest shown-test
  (testing "a series shows only the parts that are among the posts, in its order"
    (is (= {"Lisbon" ["start" "end"] "Eating" ["dinner"]}
           (series/shown all (remove (comp #{"food"} :folder) posts)))))
  (testing "a series with none of them is not shown"
    (is (= {"Eating" ["dinner"]} (series/shown all [{:folder "dinner"}])))))

(deftest series-of-test
  (testing "a post is in the series that names it, or in none"
    (is (= "Lisbon" (series/series-of all "food")))
    (is (nil? (series/series-of all "elsewhere")))))

(deftest of-post-test
  (testing "a post is the part with its number, among its parts in order, with the next part"
    (is (= {:name   "Lisbon"
            :parts  [{:folder "start" :title "The start"}
                     {:folder "food" :title "The food"}
                     {:folder "end" :title "The end"}]
            :number 1
            :next   {:folder "food" :title "The food"}}
           (series/of-post all posts "start"))))
  (testing "the last part has no next part"
    (is (nil? (:next (series/of-post all posts "end")))))
  (testing "the parts are those that the series shows, so a draft that readers do not see is no
            part for them"
    (let [published (remove (comp #{"food"} :folder) posts)
          shown     (series/shown all published)]
      (is (= [2 2] ((juxt (comp count :parts) :number) (series/of-post shown published "end"))))
      (is (= {:folder "end" :title "The end"} (:next (series/of-post shown published "start"))))
      (is (nil? (series/of-post shown published "food")))))
  (testing "a post in no series is in none"
    (is (nil? (series/of-post all posts "elsewhere")))))

(deftest problems-test
  (testing "the series are right when each part is a post or a draft, in one series"
    (is (empty? (series/problems all ["start" "food" "end" "dinner"]))))
  (testing "a part that is neither is said, with the command that keeps the series without it"
    (is (= [(str "The series \"Lisbon\" names food and end, which are no posts or drafts. Keep the "
                 "series without them with: bareblog series --name \"Lisbon\" start. A post that was "
                 "renamed goes in its place, by its new folder or title.")]
           (series/problems (dissoc all "Eating") ["start" "dinner"]))))
  (testing "a post in two series is said, with the command that takes it out of the last"
    (is (= [(str "food is in the series \"Eating\" and \"Lisbon\", and a post is in one series. Take "
                 "it out of \"Lisbon\" with: bareblog series --name \"Lisbon\" start end")]
           (series/problems (update all "Eating" conj "food") ["start" "food" "end" "dinner"])))))

(deftest text-test
  (testing "series.edn holds a series on each line, by name, and reads as the series"
    (is (= "{\"Eating\" [\"dinner\"]\n \"Lisbon\" [\"start\" \"food\" \"end\"]}\n"
           (series/text all)))
    (is (= all (edn/read-string (series/text all)))))
  (testing "a blog with no series holds an empty map"
    (is (= "{}\n" (series/text {})))))
