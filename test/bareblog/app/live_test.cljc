(ns bareblog.app.live-test
  (:require [bareblog.app.live :as live]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(deftest document-test
  (testing "the HTML of the author is the body of the document"
    (is (str/includes? (live/document "<x-icon></x-icon>" nil) "<body><x-icon></x-icon>")))
  (testing "a frame registers every BareDOM component with one module"
    (is (str/includes? (live/document "x" nil) "/dist/all.js")))
  (testing "a frame with no height tells the page the height of what it shows"
    (is (str/includes? (live/document "x" nil) live/height-key)))
  (testing "a frame with a height keeps it, and tells the page nothing"
    (is (not (str/includes? (live/document "x" 300) live/height-key)))))

(deftest fit-test
  (testing "a frame takes the height that it tells, within the least and the most"
    (is (= [48 300 1200] (map live/fit [10 300 5000])))))
