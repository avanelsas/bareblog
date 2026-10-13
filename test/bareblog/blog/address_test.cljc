(ns bareblog.blog.address-test
  (:require [bareblog.blog.address :as address]
            [clojure.test :refer [deftest is testing]]))

(deftest of-blog-test
  (testing "the address in the settings says where the blog is: its origin, its host and its top"
    (is (= {:origin "https://avanelsas.github.io" :host "avanelsas.github.io" :root "/my-blog/"}
           (address/of-blog "https://avanelsas.github.io/my-blog/"))))
  (testing "the top has a slash at its end, also when the address has none"
    (is (= "/my-blog/" (:root (address/of-blog "https://avanelsas.github.io/my-blog"))))
    (is (= "/a/b/" (:root (address/of-blog "http://example.com/a/b")))))
  (testing "an address with no path is at the top of its host"
    (is (= {:origin "https://notes.example.com" :host "notes.example.com" :root "/"}
           (address/of-blog "https://notes.example.com")))
    (is (= "http://localhost:8080" (:origin (address/of-blog "http://localhost:8080/")))))
  (testing "no address, and what is no address, have no host and are at the top"
    (is (= {:origin "" :host nil :root "/"} (address/of-blog nil)))
    (is (= {:origin "" :host nil :root "/"} (address/of-blog "not an address")))))

(deftest decoded-test
  (testing "a percent escape is read as its character, and a plus stays a plus"
    (is (= "web components" (address/decoded "web%20components")))
    (is (= "a+b" (address/decoded "a+b")))
    (is (= "café" (address/decoded (address/encoded "café")))))
  (testing "a broken escape gives nothing"
    (is (nil? (address/decoded "a%zz")))))

(deftest home-test
  (testing "the first page of the home page is the top of the blog"
    (is (= "/" (address/home "/" 1)))
    (is (= "/my-blog/" (address/home "/my-blog/" 1))))
  (testing "a later page has its number"
    (is (= "/page/2/" (address/home "/" 2)))
    (is (= "/my-blog/page/2/" (address/home "/my-blog/" 2)))))

(deftest pages-test
  (is (= "/a-week-in-lisbon/" (address/post "/" "a-week-in-lisbon")))
  (is (= "/my-blog/a-week-in-lisbon/" (address/post "/my-blog/" "a-week-in-lisbon")))
  (is (= "/archive/" (address/archive "/")))
  (is (= "/my-blog/site.edn" (address/site-data "/my-blog/")))
  (is (= "/feed.xml" (address/feed "/"))))

(deftest tag-test
  (testing "a tag is one name of its address"
    (is (= "/tags/travel/" (address/tag "/" "travel"))))
  (testing "a tag with a space, a slash or an accent is one name too"
    (is (= "/tags/web%20components/" (address/tag "/" "web components")))
    (is (= "/tags/a%2Fb/" (address/tag "/" "a/b")))
    (is (= "/my-blog/tags/caf%C3%A9/" (address/tag "/my-blog/" "café")))))

(deftest of-page-test
  (testing "every kind of page has an address"
    (is (= ["/b/" "/b/page/2/" "/b/tags/travel/" "/b/archive/" "/b/a-post/" "/b/404.html"]
           (mapv (partial address/of-page "/b/")
                 [{:kind :home :number 1} {:kind :home :number 2} {:kind :tag :tag "travel"}
                  {:kind :archive} {:kind :post :folder "a-post"} {:kind :not-found}])))))

(deftest query-test
  (testing "what an address asks for is written after a question mark, each name with its text"
    (is (= "?q=lisbon" (address/query {:q "lisbon"})))
    (is (= "?q=two%20words&tag=web" (address/query {:tag "web" :q "two words"}))))
  (testing "a name with no text is left out"
    (is (= "?tag=web" (address/query {:q "" :tag "web"})))
    (is (= "?q=x" (address/query {:q "x" :tag nil}))))
  (testing "nothing to ask for is no text at all"
    (is (= "" (address/query {})))
    (is (= "" (address/query {:q "" :tag ""})))))
