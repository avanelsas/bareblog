(ns bareblog.command.series-test
  (:require [babashka.fs :as fs]
            [bareblog.command.series :as series]
            [bareblog.example :as example]
            [bareblog.read.source :as source]
            [clojure.test :refer [deftest is testing]]))

(def ^:private posts
  [{:folder "start" :title "The start"}
   {:folder "food" :title "The food"}
   {:folder "end" :title "The end"}
   {:folder "twin-a" :title "Twins"}
   {:folder "twin-b" :title "Twins"}])

(def ^:private all {"Lisbon" ["start" "food"]})

(deftest plan-test
  (testing "a post is named by its folder, or by its title in any case"
    (is (= {:series {"Lisbon" ["start" "food"] "New" ["end" "twin-a"]} :before nil :after ["end" "twin-a"]}
           (series/plan all posts "New" ["THE END " "twin-a"]))))
  (testing "naming a series anew replaces its parts, in the new order"
    (is (= {:series {"Lisbon" ["food" "end"]} :before ["start" "food"] :after ["food" "end"]}
           (series/plan all posts "Lisbon" ["food" "The end"]))))
  (testing "a series named as it is changes nothing"
    (is (= {:unchanged? true} (series/plan all posts "Lisbon" ["start" "The food"]))))
  (testing "a series named with no posts is removed"
    (is (= {:series {} :before ["start" "food"] :after []} (series/plan all posts "Lisbon" []))))
  (testing "what cannot be done is refused"
    (is (re-find #"no series \"Prague\"" (:refusal (series/plan all posts "Prague" []))))
    (is (re-find #"Give the series a name" (:refusal (series/plan all posts " " ["start"]))))
    (is (re-find #"title or the folder \"Nothing\"" (:refusal (series/plan all posts "New" ["Nothing"]))))
    (is (re-find #"\"Twins\" is the title of twin-a and twin-b" (:refusal (series/plan all posts "New" ["Twins"]))))
    (is (re-find #"names end more than once" (:refusal (series/plan all posts "New" ["end" "The end"]))))
    (is (= "start is a part of the series \"Lisbon\", and a post is in one series."
           (:refusal (series/plan all posts "New" ["end" "start"]))))))

(def ^:private run example/run-command)

(deftest list-test
  (example/with-copy {}
    (fn [dir]
      (testing "every series is said with its parts in order, and a draft is marked"
        (is (= {:exit 0
                :said ["Getting to know BareBlog"
                       "  1. Hello bareblog (hello-bareblog)"
                       "  2. Every kind of block (every-kind)"
                       "  3. A new idea (a-new-idea, a draft)"]}
               (run series/list! dir nil))))
      (testing "a part whose folder is gone is marked, and the problem is said"
        (fs/move (fs/path dir "posts" "every-kind") (fs/path dir "posts" "every-block"))
        (let [{:keys [said]} (run series/list! dir nil)]
          (is (= "  2. every-kind (gone)" (nth said 2)))
          (is (re-find #"bareblog series --name \"Getting to know BareBlog\" hello-bareblog a-new-idea"
                       (last said)))))
      (testing "a blog with no series says how to make one"
        (fs/delete (fs/path dir "series.edn"))
        (is (re-find #"There is no series" (first (:said (run series/list! dir nil)))))))))

(deftest make-test
  (example/with-copy {}
    (fn [dir]
      (testing "a new series is shown, and written once the author says yes"
        (let [{:keys [exit said]} (run series/make! dir true "Trip" ["A week in Lisbon" "why-web-components"])]
          (is (zero? exit))
          (is (= ["A new series \"Trip\":"
                  "  1. A week in Lisbon (a-week-in-lisbon)"
                  "  2. Why web components (why-web-components)"]
                 (take 3 said)))
          (is (= ["a-week-in-lisbon" "why-web-components"] (get (source/read-series dir) "Trip")))))
      (testing "a no changes nothing"
        (let [{:keys [exit said]} (run series/make! dir false "Trip" ["why-web-components"])]
          (is (= 1 exit))
          (is (= "Nothing was changed." (last said)))
          (is (= ["a-week-in-lisbon" "why-web-components"] (get (source/read-series dir) "Trip")))))
      (testing "a series that is changed shows what it is and what it becomes"
        (let [{:keys [said]} (run series/make! dir true "Trip" ["why-web-components"])]
          (is (= ["The series \"Trip\" is now:" "  1. A week in Lisbon (a-week-in-lisbon)"
                  "  2. Why web components (why-web-components)" "It becomes:"
                  "  1. Why web components (why-web-components)"]
                 (take 5 said)))
          (is (= ["why-web-components"] (get (source/read-series dir) "Trip")))))
      (testing "a series that is removed keeps its posts, and the other series"
        (let [{:keys [exit said]} (run series/make! dir true "Trip" [])]
          (is (zero? exit))
          (is (re-find #"removed, and its posts stay" (last said)))
          (is (= ["Getting to know BareBlog"] (keys (source/read-series dir))))
          (is (fs/exists? (fs/path dir "posts" "why-web-components" "post.md")))))
      (testing "a refusal writes nothing"
        (let [before (slurp (fs/file dir "series.edn"))]
          (is (= 1 (:exit (run series/make! dir true "Trip" ["Nothing here"]))))
          (is (= before (slurp (fs/file dir "series.edn")))))))))
