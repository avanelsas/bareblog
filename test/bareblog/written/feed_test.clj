(ns bareblog.written.feed-test
  (:require [bareblog.written.feed :as feed]
            [clojure.data.xml :as xml]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def root "/my-blog/")

(def site
  {:blog  {:title "Bare <notes>" :address "https://avanelsas.github.io/my-blog/"}
   :posts [{:folder "c" :title "Tom & Jerry" :date "2026-03-01T09:30:00+01:00" :tags ["travel" "food"] :summary "Three < \"four\"."}
           {:folder "b" :title "Second" :date "2026-02-01T10:00:00+01:00"}
           {:folder "a" :title "No date"}]})

(defn- read-feed
  "The feed of `site` at `root`, read back as a document."
  [root site]
  (xml/parse-str (feed/atom-feed root site) :namespace-aware false))

(defn- children
  "The elements with the tag `tag` in `element`."
  [element tag]
  (filter (comp #{tag} :tag) (:content element)))

(defn- text-of
  "The text of the first element with the tag `tag` in `element`."
  [element tag]
  (str/join (:content (first (children element tag)))))

(deftest tree-test
  (testing "the feed is a tree: an element is its tag, its attributes and what it holds"
    (is (= [:feed {:xmlns "http://www.w3.org/2005/Atom" :xml:lang "en"}
            [:title {} "Bare"]
            [:id {} "/"]
            [:link {:href "/"}]
            [:link {:rel "self" :href "/feed.xml"}]
            [:updated {} "2026-01-01T09:00:00Z"]
            [:author {} [:name {} "Bare"]]
            [:entry {}
             [:title {} "A"]
             [:link {:href "/a/"}]
             [:id {} "/a/"]
             [:updated {} "2026-01-01T09:00:00Z"]
             nil
             [:category {:term "x"}]]]
           (feed/tree "/" {:blog {:title "Bare"} :posts [{:folder "a" :title "A" :date "2026-01-01T09:00:00Z" :tags ["x"]}]})))))

(defn- feed-attributes
  "The attributes of the feed of a blog with the settings `blog` and no posts."
  [blog]
  (second (feed/tree "/" {:blog blog :posts []})))

(deftest language-test
  (testing "the feed is in the language of the blog, and in English when the blog names none"
    (is (= "nl" (:xml:lang (feed-attributes {:title "Bare" :language "nl"}))))
    (is (= "en" (:xml:lang (feed-attributes {:title "Bare"}))))))

(deftest feed-test
  (let [document (read-feed root site)
        [c b]    (children document :entry)]
    (testing "the feed is an Atom document that names the blog, its address and itself"
      (is (str/starts-with? (feed/atom-feed root site) "<?xml version=\"1.0\" encoding=\"utf-8\"?>"))
      (is (= :feed (:tag document)))
      (is (= "http://www.w3.org/2005/Atom" (get-in document [:attrs :xmlns])))
      (is (= "https://avanelsas.github.io/my-blog/" (text-of document :id)))
      (is (= [{:href "https://avanelsas.github.io/my-blog/"}
              {:rel "self" :href "https://avanelsas.github.io/my-blog/feed.xml"}]
             (map :attrs (children document :link)))))
    (testing "what the blog wrote is read back as it was written"
      (is (= "Bare <notes>" (text-of document :title)))
      (is (= "Tom & Jerry" (text-of c :title)))
      (is (= "Three < \"four\"." (text-of c :summary))))
    (testing "the feed was changed when its newest post was"
      (is (= "2026-03-01T09:30:00+01:00" (text-of document :updated))))
    (testing "a post has its full address, its date as a moment, and its tags"
      (is (= "https://avanelsas.github.io/my-blog/c/" (text-of c :id)))
      (is (= {:href "https://avanelsas.github.io/my-blog/c/"} (:attrs (first (children c :link)))))
      (is (= "2026-02-01T10:00:00+01:00" (text-of b :updated)))
      (is (= ["travel" "food"] (map (comp :term :attrs) (children c :category)))))
    (testing "a post with no summary has none, and a post with no date is left out"
      (is (empty? (children b :summary)))
      (is (= 2 (count (children document :entry)))))
    (testing "the author is the one the blog names, or else the blog"
      (is (= "Bare <notes>" (text-of (first (children document :author)) :name)))
      (is (= "Alexander"
             (text-of (first (children (read-feed root (assoc-in site [:blog :author] "Alexander")) :author))
                      :name)))))
  (testing "a blog that names no address has a feed with addresses from the top of its host"
    (let [document (read-feed "/" {:blog {:title "Bare"} :posts [{:folder "a" :title "A" :date "2026-01-01"}]})]
      (is (= "/a/" (text-of (first (children document :entry)) :id)))))
  (testing "a blog with no dated post still says when it was changed"
    (is (= "1970-01-01T00:00:00Z" (text-of (read-feed "/" {:blog {:title "Bare"} :posts []}) :updated)))))
