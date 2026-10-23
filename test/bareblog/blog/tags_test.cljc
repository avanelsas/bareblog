(ns bareblog.blog.tags-test
  (:require [bareblog.blog.tags :as tags]
            [clojure.test :refer [deftest is testing]]))

(def posts
  [{:folder "c" :tags ["BareDOM" "web-components"]}
   {:folder "b" :tags ["baredom" "Web Components" "travel"]}
   {:folder "a" :tags ["BareDOM"]}])

(deftest folded-test
  (testing "a tag is the same tag in any case, and with a dash, an underscore or a space between its words"
    (is (apply = (map tags/folded ["web components" "Web-Components" "web_components" "web  components"]))))
  (testing "a tag with other letters is another tag"
    (is (not= (tags/folded "cljs") (tags/folded "clojurescript")))))

(deftest collisions-test
  (testing "each tag written in more than one way gives its spellings, with the folders of each"
    (is (= [{"BareDOM" ["c" "a"] "baredom" ["b"]}
            {"Web Components" ["b"] "web-components" ["c"]}]
           (tags/collisions posts))))
  (testing "a blog with each tag written one way has none"
    (is (empty? (tags/collisions [{:folder "a" :tags ["travel"]} {:folder "b" :tags ["travel"]}])))))

(deftest preferred-test
  (testing "the spelling of most posts is kept"
    (is (= "BareDOM" (tags/preferred {"BareDOM" ["c" "a"] "baredom" ["b"]})))
    (is (= "baredom" (tags/preferred {"BareDOM" ["c"] "baredom" ["b" "a"]}))))
  (testing "of spellings with as many posts, the first in the order of the alphabet is kept"
    (is (= "Web Components" (tags/preferred {"Web Components" ["b"] "web-components" ["c"]})))))

(deftest remedy-test
  (let [spellings (sorted-map "BareDOM" ["c" "a"] "baredom" ["b"] "Baredom" ["d"])]
    (testing "the other spellings are renamed as the preferred one"
      (is (= {:renames [["Baredom" "BareDOM"] ["baredom" "BareDOM"]] :publish? false}
             (tags/remedy #{} spellings))))
    (testing "a post as it was published among them is published with the others"
      (is (= {:renames [["Baredom" "BareDOM"] ["baredom" "BareDOM"]] :publish? true}
             (tags/remedy #{"b"} spellings))))
    (testing "when every post is as it was published, no rename is named, since it may be done"
      (is (= {:renames [] :publish? true} (tags/remedy #{"a" "b" "c" "d"} spellings))))))

(deftest problems-test
  (testing "a tag written in more than one way is said with its posts, and the renames that write it one way"
    (is (= [(str "The tags \"BareDOM\" in c, a and \"baredom\" in b are one tag written in different ways, "
                 "and cannot each have a page. Write them one way with: bareblog tags rename \"baredom\" \"BareDOM\"")]
           (tags/problems [{:folder "c" :tags ["BareDOM"]} {:folder "b" :tags ["baredom"]} {:folder "a" :tags ["BareDOM"]}]
                          #{}))))
  (testing "a spelling that a post as it was published has is said as that, and the posts are published together"
    (let [[problem] (tags/problems [{:folder "new" :tags ["BareDOM"]} {:folder "old" :tags ["baredom"]}] #{"old"})]
      (is (re-find #"\"baredom\" in old \(as it was published\)" problem))
      (is (re-find #"rename \"baredom\" \"BareDOM\", and publish them together with: bareblog publish --all$" problem))))
  (testing "a tag whose posts are all as they were published is published again once it is written one way"
    (let [[problem] (tags/problems [{:folder "a" :tags ["BareDOM"]} {:folder "b" :tags ["baredom"]}] #{"a" "b"})]
      (is (re-find #"Once they are written one way on disk, which bareblog tags shows, publish them together with: bareblog publish --all$"
                   problem))
      (is (not (re-find #"rename" problem)))))
  (testing "a blog with each tag written one way has nothing wrong with its tags"
    (is (empty? (tags/problems [{:folder "a" :tags ["travel"]}] #{})))))

(deftest rename-refusal-test
  (let [all ["BareDOM" "baredom" "clojurescript" "cljs" "travel"]]
    (testing "a tag can be renamed as another spelling of itself, as a new tag, or as a tag that is there"
      (is (nil? (tags/rename-refusal all "baredom" "BareDOM")))
      (is (nil? (tags/rename-refusal all "travel" "journeys")))
      (is (nil? (tags/rename-refusal all "cljs" "clojurescript")))
      (is (nil? (tags/rename-refusal ["A-b" "a b" "a_B"] "a b" "A-b"))))
    (testing "a tag that no text has cannot be renamed"
      (is (re-find #"No post or page has the tag \"zebra\"" (tags/rename-refusal all "zebra" "Zebra"))))
    (testing "a tag is not renamed as itself"
      (is (re-find #"already" (tags/rename-refusal all "travel" "travel"))))
    (testing "a rename that would write a tag in another way than it is written is refused"
      (is (= (str "\"ClojureScript\" is one tag with \"clojurescript\", written in another way. "
                  "Rename as that, or rename that as well.")
             (tags/rename-refusal all "cljs" "ClojureScript")))
      (testing "a spelling that more posts have is named once"
        (is (re-find #"one tag with \"clojurescript\", written"
                     (tags/rename-refusal (into all ["clojurescript" "clojurescript"]) "cljs" "ClojureScript")))))))

(deftest renamed-test
  (testing "a renamed tag keeps its place, and is there once"
    (is (= ["a" "BareDOM" "c"] (tags/renamed ["a" "baredom" "c"] "baredom" "BareDOM")))
    (is (= ["BareDOM" "c"] (tags/renamed ["BareDOM" "baredom" "c"] "baredom" "BareDOM")))))
