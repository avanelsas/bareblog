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
        (is (= folder (get-in told [:text :folder])))
        (is (every? #(str/starts-with? % (address/post "/" folder))
                    (keep :src (tree-seq coll? seq (get-in told [:text :blocks])))))))))

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
        (is (= (:folder (first (:posts site))) (get-in alone [:text :folder])))
        (is (seq (get-in alone [:text :blocks])))
        (is (= alone with-site))))
    (testing "a blog that is served under a path names its files from there"
      (let [under (assoc blog :root "/my-blog/")
            told  (source/read-page under {:kind :post :folder "a-week-in-lisbon"})]
        (is (every? #(str/starts-with? % "/my-blog/a-week-in-lisbon/")
                    (keep :src (tree-seq coll? seq (get-in told [:text :blocks])))))))
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
      (is (= {:dir      "example"
              :root     "/my-blog/"
              :preview? false
              :settings {:address "https://avanelsas.github.io/my-blog"}}
             (source/published "example" {:address "https://avanelsas.github.io/my-blog"})))
      (is (= "/" (:root (source/published "example" {})))))
    (testing "a directory is a blog when it has settings at its top"
      (is (source/blog? "example"))
      (is (not (source/blog? "src")))
      (is (not (source/blog? nil)))
      (is (= "No blog.edn in src. A blog has one at its top." (source/refusal "src"))))
    (testing "the page of a post that is not there has no data"
      (is (nil? (source/read-page blog {:kind :post :folder "not-there"}))))))

(deftest as-published-test
  (example/with-copy {}
    (fn [dir]
      (let [blog   (source/published dir (source/read-settings dir))
            lisbon {:kind :post :folder "a-week-in-lisbon"}]
        (fs/create-dirs (fs/path dir "docs" "a-week-in-lisbon"))
        (spit (fs/file dir "docs" "site.edn")
              (pr-str {:blog {:title "As published"} :posts [{:folder "a-week-in-lisbon" :title "Published" :date "2020-01-01"}]}))
        (spit (fs/file dir "docs" "a-week-in-lisbon" "page.edn")
              (pr-str {:page lisbon :text {:folder "a-week-in-lisbon" :title "Published" :blocks []}}))
        (testing "the site as it was published last is the one in the output directory"
          (is (= "As published" (get-in (source/read-published dir) [:blog :title])))
          (is (nil? (source/read-published (str (fs/path dir "posts"))))))
        (testing "a post that is taken as published is the published one, in the list and on its page"
          (let [kept (assoc blog :as-published #{"a-week-in-lisbon"} :read #{"hello-bareblog"})]
            (is (= #{"Published" "hello-bareblog"} (set (map :title (:posts (source/read-site kept))))))
            (is (= "Published" (get-in (source/read-page kept lisbon) [:text :title])))
            (is (= "A week in Lisbon" (get-in (source/read-page blog lisbon) [:text :title])))))
        (testing "its files are the published ones too"
          (is (= (str (fs/path dir "docs" "a-week-in-lisbon"))
                 (str (source/find-media-dir (assoc blog :as-published #{"a-week-in-lisbon"}) "a-week-in-lisbon"))))
          (is (= (str (fs/path dir "posts" "a-week-in-lisbon"))
                 (str (source/find-media-dir blog "a-week-in-lisbon")))))
        (testing "only the folders that are named are read, and none when none is named"
          (is (= ["hello-bareblog"] (map :folder (:posts (source/read-site (assoc blog :read #{"hello-bareblog"}))))))
          (is (empty? (:posts (source/read-site (assoc blog :read #{}))))))
        (testing "a folder with a name that is not allowed is said, whatever is read"
          (fs/create-dirs (fs/path dir "posts" "Bad Name"))
          (is (some #(re-find #"Bad Name" %) (:problems (source/read-site (assoc blog :read #{"hello-bareblog"})))))
          (is (empty? (source/read-problems (assoc blog :read #{"hello-bareblog"})))))
        (testing "a preview shows a post with the date it was published with, when its text gives none"
          (spit (fs/file dir "docs" "site.edn")
                (pr-str {:blog {} :posts [{:folder "hello-bareblog" :date "2026-09-09"}
                                          {:folder "a-week-in-lisbon" :date "1999-01-01"}]}))
          (let [entry (fn [folder] (first (filter (comp #{folder} :folder)
                                                  (:posts (source/read-site (source/preview dir))))))]
            (is (= "2026-09-09" (:date (entry "hello-bareblog"))))
            (is (= "2026-10-11" (:date (entry "a-week-in-lisbon"))))
            (is (nil? (:date (entry "a-new-idea"))))
            (is (= "2026-09-09"
                   (get-in (source/read-page (source/preview dir) {:kind :post :folder "hello-bareblog"})
                           [:text :date])))))
        (testing "a post whose text gives no date gets the one that the blog gives it, and no other"
          (let [dated (assoc blog :dates {"hello-bareblog" "2026-10-12" "a-week-in-lisbon" "1999-01-01"})
                entry (fn [folder] (first (filter (comp #{folder} :folder) (:posts (source/read-site dated)))))]
            (is (= "2026-10-12" (:date (entry "hello-bareblog"))))
            (is (= "2026-10-11" (:date (entry "a-week-in-lisbon"))))
            (is (= "2026-10-12" (get-in (source/read-page dated {:kind :post :folder "hello-bareblog"}) [:text :date])))))))))

(deftest pages-test
  (example/with-copy {}
    (fn [dir]
      (let [blog  (source/preview dir)
            about {:kind :post :folder "about"}]
        (testing "a folder among the pages is a page of the blog, and no post"
          (is (= ["about"] (map :folder (:pages (source/read-site blog)))))
          (is (not-any? #{"about"} (map :folder (:posts (source/read-site blog))))))
        (testing "its entry has its title and its order, and says that it is not published"
          (is (= {:folder "about" :title "About" :order 1 :unpublished? true}
                 (select-keys (first (:pages (source/read-site blog))) [:folder :title :order :unpublished?]))))
        (testing "its page is asked for as that of a post, and is told that it is a page of the author"
          (let [told (source/read-page blog about)]
            (is (= {:kind :standalone :folder "about"} (:page told)))
            (is (= "About" (get-in told [:text :title])))
            (is (nil? (get-in told [:text :date])))
            (is (= ["/about/boat.svg"] (keep :src (tree-seq coll? seq (get-in told [:text :blocks])))))))
        (testing "a post is still told that it is a post"
          (is (= :post (get-in (source/read-page blog {:kind :post :folder "a-week-in-lisbon"}) [:page :kind]))))
        (testing "a page is read for the readers too, and is then not marked"
          (let [published (source/published dir (source/read-settings dir))]
            (is (= [{:folder "about" :title "About"}]
                   (map #(select-keys % [:folder :title :unpublished?]) (:pages (source/read-site published)))))))
        (testing "a page that is in the published list is not marked in a preview"
          (fs/create-dirs (fs/path dir "docs"))
          (spit (fs/file dir "docs" "site.edn") (pr-str {:blog {} :posts [] :pages [{:folder "about"}]}))
          (is (not (contains? (first (:pages (source/read-site blog))) :unpublished?)))
          (fs/delete-tree (fs/path dir "docs")))
        (testing "the files beside a page are found where the page is"
          (is (= (str (fs/path dir "pages" "about")) (str (source/find-dir blog "about")))))
        (testing "a page with the name of a post is left out, and the author is told"
          (fs/create-dirs (fs/path dir "pages" "hello-bareblog"))
          (spit (fs/file dir "pages" "hello-bareblog" "page.md") "# Not shown\n")
          (let [site (source/read-site blog)]
            (is (= ["about"] (map :folder (:pages site))))
            (is (some #(re-find #"pages/hello-bareblog is left out" %) (:problems site)))
            (is (= :post (get-in (source/read-page blog {:kind :post :folder "hello-bareblog"}) [:page :kind])))))
        (testing "a page with an order that is no number is something wrong with the blog"
          (spit (fs/file dir "pages" "about" "page.md") "---\norder: first\n---\n\n# About\n")
          (is (some #(re-find #"pages/about: The order" %) (source/read-problems (assoc blog :read #{"about"})))))))))

