(ns bareblog.address-test
  (:require [bareblog.address :as address]
            [clojure.test :refer [deftest is testing]]))

(deftest home-test
  (testing "the first page of the home page is the top of the blog"
    (is (= "/" (address/home 1))))
  (testing "a later page has its number"
    (is (= "/page/2/" (address/home 2)))))

(deftest pages-test
  (is (= "/a-week-in-lisbon/" (address/post "a-week-in-lisbon")))
  (is (= "/archive/" address/archive))
  (is (= "/site.edn" address/site-data)))

(deftest tag-test
  (testing "a tag is one name of its address"
    (is (= "/tags/travel/" (address/tag "travel"))))
  (testing "a tag with a space, a slash or an accent is one name too"
    (is (= "/tags/web%20components/" (address/tag "web components")))
    (is (= "/tags/a%2Fb/" (address/tag "a/b")))
    (is (= "/tags/caf%C3%A9/" (address/tag "café")))))
