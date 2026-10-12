(ns bareblog.output-test
  (:require [bareblog.output :as output]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def root "/my-blog/")

(def site
  {:blog  {:title "Bare notes" :address "https://avanelsas.github.io/my-blog/" :per-page 2}
   :posts [{:folder "c" :title "Third" :date "2026-03-01" :tags ["travel" "web components"]}
           {:folder "b" :title "Second" :date "2026-02-01" :tags ["travel"]}
           {:folder "a" :title "First" :date "2025-01-01"}]})

(deftest pages-test
  (testing "a blog has every page of its home page, the page of each tag, its archive, the page of
            each post, and the page for an address with no page"
    (is (= [{:kind :home :number 1} {:kind :home :number 2}
            {:kind :tag :tag "travel"} {:kind :tag :tag "web components"}
            {:kind :archive}
            {:kind :post :folder "c"} {:kind :post :folder "b"} {:kind :post :folder "a"}
            {:kind :not-found}]
           (output/pages site))))
  (testing "each page of the author that is no post is a page, after the posts"
    (is (= [{:kind :standalone :folder "about"} {:kind :not-found}]
           (take-last 2 (output/pages (assoc site :pages [{:folder "about" :title "About"}])))))
    (is (= ["about/index.html" "about/page.edn"]
           [(output/file-of root {:kind :standalone :folder "about"})
            (output/data-of root {:kind :standalone :folder "about"})])))
  (testing "a blog template that shows a post on its home page has one page of it"
    (is (= [{:kind :home :number 1}]
           (filter (comp #{:home} :kind) (output/pages (assoc-in site [:blog :template] :notebook))))))
  (testing "a blog with no posts still has a home page, an archive and the page for no page"
    (is (= [{:kind :home :number 1} {:kind :archive} {:kind :not-found}]
           (output/pages (assoc site :posts []))))))

(def awkward
  "A blog with tags that are hard to make a directory of."
  (assoc site :posts [{:folder "a" :title "A" :tags ["a/b" ".." "." "../../x" "c++" "web components" "100%" "café"]}]))

(deftest pageless-test
  (testing "a tag with a slash in it, or of dots alone, or of nothing, cannot have a page"
    (is (every? output/pageless-tag? ["a/b" "a\\b" "." ".." "..." ""]))
    (is (not-any? output/pageless-tag? ["travel" "web components" "c++" "v1.2" ".net" "café"])))
  (testing "a tag that cannot have a page is something wrong with the blog, said once"
    (is (= 1 (count (output/problems {:posts [{:tags ["a/b"]} {:tags ["a/b" "ok"]}]}))))
    (is (re-find #"a/b" (first (output/problems {:posts [{:tags ["a/b"]}]}))))
    (is (empty? (output/problems site)))))

(deftest awkward-tags-test
  (testing "a tag that cannot have a page gets none"
    (is (= ["100%" "c++" "café" "web components"]
           (mapv :tag (filter (comp #{:tag} :kind) (output/pages awkward))))))
  (testing "the file of every page, and of its data, is below the top of the blog"
    (doseq [page (output/pages awkward)
            file (remove nil? [(output/file-of root page) (output/data-of root page)])]
      (is (not (str/starts-with? file "/")) file)
      (is (not-any? #{".." "." ""} (butlast (str/split file #"/"))) file))))

(deftest files-test
  (testing "the page of a directory is the index of that directory, from the top of the blog"
    (is (= ["index.html" "page/2/index.html" "archive/index.html" "c/index.html"]
           (mapv (partial output/file-of "/my-blog/")
                 [{:kind :home :number 1} {:kind :home :number 2} {:kind :archive} {:kind :post :folder "c"}]))))
  (testing "the directory of a tag has the name of the tag as it is written"
    (is (= "tags/web components/index.html" (output/file-of "/my-blog/" {:kind :tag :tag "web components"})))
    (is (= "tags/café/index.html" (output/file-of "/" {:kind :tag :tag "café"}))))
  (testing "the page for an address with no page is a file at the top"
    (is (= "404.html" (output/file-of "/my-blog/" {:kind :not-found}))))
  (testing "the data of a page is beside the page"
    (is (= ["page.edn" "page/2/page.edn" "tags/travel/page.edn" "c/page.edn"]
           (mapv (partial output/data-of "/my-blog/")
                 [{:kind :home :number 1} {:kind :home :number 2} {:kind :tag :tag "travel"}
                  {:kind :post :folder "c"}]))))
  (testing "the page for an address with no page has no data"
    (is (nil? (output/data-of "/my-blog/" {:kind :not-found})))))

(deftest texts-test
  (let [texts (output/texts root site)]
    (testing "a blog on the domain that GitHub gives has the data of its site, its feed, and the file
              that has its files sent as they are"
      (is (= #{"site.edn" "feed.xml" ".nojekyll"} (set (keys texts)))))
    (testing "the data of the site is the site"
      (is (= site (edn/read-string (texts "site.edn")))))
    (testing "the readers are told nothing of what is wrong with the blog"
      (is (= site (edn/read-string (get (output/texts root (assoc site :problems ["A is left out."]))
                                        "site.edn")))))
    (testing "the feed is the feed of the site"
      (is (str/includes? (texts "feed.xml") "https://avanelsas.github.io/my-blog/c/"))))
  (testing "a blog on a domain of the author names it"
    (is (= "notes.example.com\n"
           (get (output/texts "/" (assoc-in site [:blog :address] "https://notes.example.com/")) "CNAME"))))
  (testing "a blog that names no address names no domain"
    (is (not (contains? (output/texts "/" (update site :blog dissoc :address)) "CNAME")))))
