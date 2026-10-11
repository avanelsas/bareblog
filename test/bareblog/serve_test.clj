(ns bareblog.serve-test
  (:require [bareblog.address :as address]
            [bareblog.post :as post]
            [bareblog.serve :as serve]
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
