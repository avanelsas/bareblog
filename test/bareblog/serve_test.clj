(ns bareblog.serve-test
  (:require [babashka.fs :as fs]
            [bareblog.address :as address]
            [bareblog.app.templates :as templates]
            [bareblog.app.view :as view]
            [bareblog.post :as post]
            [bareblog.serve :as serve]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(deftest page-routes-test
  (testing "the top of the blog is the first page of the home page"
    (is (= {:serve :page :page {:kind :home :number 1}} (serve/route "/"))))
  (testing "a later page of the home page has its number"
    (is (= {:serve :page :page {:kind :home :number 2}} (serve/route "/page/2/")))
    (is (= {:serve :nothing} (serve/route "/page/0/")))
    (is (= {:serve :nothing} (serve/route "/page/two/"))))
  (testing "the page of a tag has the tag as it is written, not as its address spells it"
    (is (= {:serve :page :page {:kind :tag :tag "travel"}} (serve/route "/tags/travel/")))
    (is (= {:serve :page :page {:kind :tag :tag "web components"}} (serve/route "/tags/web%20components/"))))
  (testing "a slash that is spelled as an escape is part of the tag"
    (is (= {:serve :page :page {:kind :tag :tag "a/b"}} (serve/route "/tags/a%2Fb/")))
    (is (= {:serve :nothing} (serve/route "/tags/a/b/"))))
  (testing "the server has the page of every tag that the app links to"
    (doseq [tag-name ["travel" "web components" "a/b" "c++" "100%" "café" "what?"]]
      (is (= {:serve :page :page {:kind :tag :tag tag-name}} (serve/route (address/tag tag-name))))))
  (testing "a tag with a broken escape serves nothing"
    (is (= {:serve :nothing} (serve/route "/tags/%zz/"))))
  (testing "the archive"
    (is (= {:serve :page :page {:kind :archive}} (serve/route "/archive/"))))
  (testing "the address of a post serves its page"
    (is (= {:serve :page :page {:kind :post :folder "a-week-in-lisbon"}} (serve/route "/a-week-in-lisbon/"))))
  (testing "the data and the socket of a page are beside the page, for every kind of page"
    (is (= {:serve :data :page {:kind :home :number 1}} (serve/route "/page.edn")))
    (is (= {:serve :changes :page {:kind :home :number 2}} (serve/route "/page/2/changes")))
    (is (= {:serve :data :page {:kind :tag :tag "travel"}} (serve/route "/tags/travel/page.edn")))
    (is (= {:serve :changes :page {:kind :archive}} (serve/route "/archive/changes")))
    (is (= {:serve :data :page {:kind :post :folder "a-post"}} (serve/route "/a-post/page.edn")))
    (is (= {:serve :changes :page {:kind :post :folder "a-post"}} (serve/route "/a-post/changes"))))
  (testing "the address of a page without its last slash redirects to it"
    (is (= {:serve :redirect :to "/a-week-in-lisbon/"} (serve/route "/a-week-in-lisbon")))
    (is (= {:serve :redirect :to "/archive/"} (serve/route "/archive")))
    (is (= {:serve :redirect :to "/page/2/"} (serve/route "/page/2")))))

(deftest file-routes-test
  (testing "the data of the site is at the top"
    (is (= {:serve :site} (serve/route "/site.edn"))))
  (testing "any other file beside a post is the media of that post"
    (is (= {:serve :media :folder "a-week-in-lisbon" :file "harbour.svg"}
           (serve/route "/a-week-in-lisbon/harbour.svg"))))
  (testing "a percent escape in the name of a file is read as its character"
    (is (= {:serve :media :folder "a-post" :file "my photo.jpg"} (serve/route "/a-post/my%20photo.jpg")))
    (is (= {:serve :media :folder "a-post" :file "café.jpg"} (serve/route "/a-post/caf%C3%A9.jpg"))))
  (testing "the server sends every file that a post can name beside it"
    (doseq [source ["harbour.jpg" "my%20photo.jpg" "caf%C3%A9.jpg" "a+b.jpg" "photo.final.JPG"]]
      (is (= {:serve :media :folder "a-post" :file (post/file-beside source)}
             (serve/route (str "/a-post/" source))))))
  (testing "a file at the top, and anything under js, is the app"
    (is (= {:serve :static :file "journal.css"} (serve/route "/journal.css")))
    (is (= {:serve :static :file "js/cljs-runtime/cljs.core.js"}
           (serve/route "/js/cljs-runtime/cljs.core.js")))))

(deftest nothing-test
  (testing "a path that names nothing serves nothing"
    (is (= {:serve :nothing} (serve/route "/a/b/c/d")))
    (is (= {:serve :nothing} (serve/route "/Not-A-Folder/"))))
  (testing "a hidden file and a broken escape serve nothing"
    (is (= {:serve :nothing} (serve/route "/a-post/.env")))
    (is (= {:serve :nothing} (serve/route "/a-post/a%zz.jpg"))))
  (testing "a path with two dots in a row serves nothing"
    (is (= {:serve :nothing} (serve/route "/js/../../blog.edn")))
    (is (= {:serve :nothing} (serve/route "/a-post/..")))
    (is (= {:serve :nothing} (serve/route "/a-post/%2e%2e")))))

;; What the server reads of a blog on disk. The example blog is the blog.

(def ^:private example "example")

(defn- read-copy-as
  "What `read` gives for a copy of the example blog with the blog template `template-name`.
   The copy is gone afterwards."
  [template-name read]
  (let [dir (fs/create-temp-dir)]
    (try
      (fs/copy-tree example dir)
      (spit (fs/file dir "blog.edn")
            (pr-str (assoc (edn/read-string (slurp (fs/file example "blog.edn"))) :template template-name)))
      (read (str dir))
      (finally
        (fs/delete-tree dir)))))

(defn- post-shown
  "What the page `told` shows of the post, for `site` as a blog with the blog template
   `template-name`."
  [site told template-name]
  (get-in (view/page (assoc-in site [:blog :template] template-name) told {})
          [:lists :main :by-key "post"]))

;; The blog is read once, and the name of the blog template is then the one thing that
;; differs. This holds the view to showing a post the same way whatever the blog template. It
;; does not show that a page looks right, which is seen in a browser.
(deftest every-blog-template-shows-the-same-posts
  (let [site (serve/read-site example)]
    (testing "the example blog has posts to show"
      (is (seq (:posts site))))
    (doseq [{:keys [folder]} (:posts site)
            :let             [told  (serve/read-page example {:kind :post :folder folder})
                              shown (map (partial post-shown site told) (keys templates/by-name))]]
      (testing (str folder " is shown, with the same title, tags and blocks under every blog template")
        (is (some? (first shown)))
        (is (apply = shown)))
      (testing (str folder " names its folder, and shows its files by their full address")
        (is (= folder (get-in told [:post :folder])))
        (is (every? #(str/starts-with? % (address/post folder))
                    (keep :src (tree-seq coll? seq (get-in told [:post :blocks])))))))))

(deftest home-page-test
  (let [home {:kind :home :number 1}]
    (testing "a home page that lists entries is told no post"
      (is (= {:page home} (serve/read-page example home))))
    (testing "a home page that shows the newest post is told that post, with or without the site
              as it was read"
      (let [[site alone with-site] (read-copy-as :notebook
                                                 (fn [root]
                                                   (let [site (serve/read-site root)]
                                                     [site
                                                      (serve/read-page root home)
                                                      (serve/read-page root home site)])))]
        (is (= (:folder (first (:posts site))) (get-in alone [:post :folder])))
        (is (seq (get-in alone [:post :blocks])))
        (is (= alone with-site))))
    (testing "the page of a post that is not there has no data"
      (is (nil? (serve/read-page example {:kind :post :folder "not-there"}))))))

