(ns bareblog.serve-test
  (:require [bareblog.serve :as serve]
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
