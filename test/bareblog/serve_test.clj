(ns bareblog.serve-test
  (:require [bareblog.post :as post]
            [bareblog.serve :as serve]
            [clojure.test :refer [deftest is testing]]))

(deftest route-test
  (testing "the address of a post serves the page"
    (is (= {:serve :page :folder "a-week-in-lisbon"} (serve/route "/a-week-in-lisbon/"))))
  (testing "the address of a post without its last slash redirects to it"
    (is (= {:serve :redirect :folder "a-week-in-lisbon"} (serve/route "/a-week-in-lisbon"))))
  (testing "the data of a page is beside the page"
    (is (= {:serve :data :folder "a-week-in-lisbon"} (serve/route "/a-week-in-lisbon/page.edn"))))
  (testing "any other file beside a page is the media of its post"
    (is (= {:serve :media :folder "a-week-in-lisbon" :file "harbour.svg"}
           (serve/route "/a-week-in-lisbon/harbour.svg"))))
  (testing "a percent escape in the name of a file is read as its character"
    (is (= {:serve :media :folder "a-post" :file "my photo.jpg"} (serve/route "/a-post/my%20photo.jpg")))
    (is (= {:serve :media :folder "a-post" :file "café.jpg"} (serve/route "/a-post/caf%C3%A9.jpg"))))
  (testing "the server sends every file that a post can name beside it"
    (doseq [source ["harbour.jpg" "my%20photo.jpg" "caf%C3%A9.jpg" "a+b.jpg" "photo.final.JPG"]]
      (is (= {:serve :media :folder "a-post" :file (post/file-beside source)}
             (serve/route (str "/a-post/" source))))))
  (testing "a hidden file and a broken escape serve nothing"
    (is (= {:serve :nothing} (serve/route "/a-post/.env")))
    (is (= {:serve :nothing} (serve/route "/a-post/a%zz.jpg")))
    (is (= {:serve :nothing} (serve/route "/a-post/%2e%2e"))))
  (testing "a file at the top, and anything under js, is the app"
    (is (= {:serve :static :file "journal.css"} (serve/route "/journal.css")))
    (is (= {:serve :static :file "js/cljs-runtime/cljs.core.js"}
           (serve/route "/js/cljs-runtime/cljs.core.js"))))
  (testing "a path that names nothing serves nothing"
    (is (= {:serve :nothing} (serve/route "/")))
    (is (= {:serve :nothing} (serve/route "/a/b/c/d"))))
  (testing "a path with two dots in a row serves nothing"
    (is (= {:serve :nothing} (serve/route "/js/../../blog.edn")))
    (is (= {:serve :nothing} (serve/route "/a-post/..")))))
