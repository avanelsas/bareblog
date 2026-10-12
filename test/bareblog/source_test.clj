(ns bareblog.source-test
  "What is read of a blog on disk. The example blog is the blog."
  (:require [babashka.fs :as fs]
            [bareblog.address :as address]
            [bareblog.app.templates :as templates]
            [bareblog.app.view :as view]
            [bareblog.example :as example]
            [bareblog.source :as source]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def ^:private blog (source/preview example/dir))

(defn- read-copy-as
  "What `read` gives for a copy of the example blog with the blog template `template-name`."
  [template-name read]
  (example/with-copy {:template template-name} (comp read source/preview)))

(defn- post-shown
  "What the page `told` shows of the post, for `site` as a blog with the blog template
   `template-name`."
  [site told template-name]
  (get-in (view/page "/" (assoc-in site [:blog :template] template-name) told {})
          [:lists :main :by-key "post"]))

;; The blog is read once, and the name of the blog template is then the one thing that
;; differs. This holds the view to showing a post the same way whatever the blog template. It
;; does not show that a page looks right, which is seen in a browser.
(deftest every-blog-template-shows-the-same-posts
  (let [site (source/read-site blog)]
    (testing "the example blog has posts to show"
      (is (seq (:posts site))))
    (doseq [{:keys [folder]} (:posts site)
            :let             [told  (source/read-page blog {:kind :post :folder folder})
                              shown (map (partial post-shown site told) (keys templates/by-name))]]
      (testing (str folder " is shown, with the same title, tags and blocks under every blog template")
        (is (some? (first shown)))
        (is (apply = shown)))
      (testing (str folder " names its folder, and shows its files by their full address")
        (is (= folder (get-in told [:post :folder])))
        (is (every? #(str/starts-with? % (address/post "/" folder))
                    (keep :src (tree-seq coll? seq (get-in told [:post :blocks])))))))))

(deftest home-page-test
  (let [home {:kind :home :number 1}]
    (testing "a home page that lists entries is told no post"
      (is (= {:page home} (source/read-page blog home))))
    (testing "a home page that shows the newest post is told that post, with or without the site
              as it was read"
      (let [[site alone with-site] (read-copy-as :notebook
                                                 (fn [blog]
                                                   (let [site (source/read-site blog)]
                                                     [site
                                                      (source/read-page blog home)
                                                      (source/read-page blog home site)])))]
        (is (= (:folder (first (:posts site))) (get-in alone [:post :folder])))
        (is (seq (get-in alone [:post :blocks])))
        (is (= alone with-site))))
    (testing "a blog that is served under a path names its files from there"
      (let [under (assoc blog :root "/my-blog/")
            told  (source/read-page under {:kind :post :folder "a-week-in-lisbon"})]
        (is (every? #(str/starts-with? % "/my-blog/a-week-in-lisbon/")
                    (keep :src (tree-seq coll? seq (get-in told [:post :blocks])))))))
    (testing "a tag that cannot have a page, and a folder that is no post, are what is wrong with a blog"
      (let [problems (example/with-copy {}
                       (fn [dir]
                         (fs/create-dirs (fs/path dir "posts" "Bad Name"))
                         (spit (fs/file dir "posts" "hello-bareblog" "post.md") "---\ntags: a/b\n---\n\n# Hello\n")
                         (:problems (source/read-site (source/preview dir)))))]
        (is (= 2 (count problems)))
        (is (re-find #"Bad Name" (first problems)))
        (is (re-find #"a/b" (second problems)))))
    (testing "a blog that is served without its drafts has none in its list, and no page for one"
      (let [published (assoc blog :preview? false)]
        (is (some :draft? (:posts (source/read-site blog))))
        (is (not-any? :draft? (:posts (source/read-site published))))
        (is (some? (source/read-page blog {:kind :post :folder "a-new-idea"})))
        (is (nil? (source/read-page published {:kind :post :folder "a-new-idea"})))))
    (testing "a blog is published at the address in its settings, without its drafts"
      (is (= {:dir "example" :root "/my-blog/" :preview? false}
             (source/published "example" {:address "https://avanelsas.github.io/my-blog"})))
      (is (= "/" (:root (source/published "example" {})))))
    (testing "a directory is a blog when it has settings at its top"
      (is (source/blog? "example"))
      (is (not (source/blog? "src")))
      (is (not (source/blog? nil)))
      (is (re-find #"bb serve" (source/refusal nil "bb serve example")))
      (is (re-find #"No blog.edn in src" (source/refusal "src" "bb serve example"))))
    (testing "the page of a post that is not there has no data"
      (is (nil? (source/read-page blog {:kind :post :folder "not-there"}))))))

