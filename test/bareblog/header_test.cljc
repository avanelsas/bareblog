(ns bareblog.header-test
  (:require [bareblog.header :as header]
            [clojure.test :refer [deftest is testing]]))

(deftest split-test
  (testing "a header block gives its lines, and the body is what follows it"
    (is (= {:lines ["title: A week in Lisbon" "tags: travel"] :body "\nWe arrived late."}
           (header/split "---\ntitle: A week in Lisbon\ntags: travel\n---\n\nWe arrived late."))))
  (testing "a text with no header block is all body"
    (is (= {:lines [] :body "We arrived late.\n\n---\n\nMore."}
           (header/split "We arrived late.\n\n---\n\nMore."))))
  (testing "a header block that never closes is body"
    (is (= {:lines [] :body "---\ntitle: Open"}
           (header/split "---\ntitle: Open"))))
  (testing "an empty text has no header lines"
    (is (= {:lines [] :body ""} (header/split "")))))

(deftest fields-test
  (testing "each field BareBlog reads is under its key"
    (is (= {:title "A week in Lisbon" :date "2026-10-11" :tags ["travel" "portugal"]
            :summary "Seven days of hills and trams."}
           (header/fields ["title: A week in Lisbon"
                           "date: 2026-10-11"
                           "tags: travel, portugal"
                           "summary: Seven days of hills and trams."]))))
  (testing "the name of a field is read in any case"
    (is (= {:title "Lisbon"} (header/fields ["Title: Lisbon"]))))
  (testing "a value keeps a colon that is in it"
    (is (= {:title "Lisbon: a week"} (header/fields ["title: Lisbon: a week"]))))
  (testing "a field BareBlog does not read is left out"
    (is (= {:title "Lisbon"} (header/fields ["title: Lisbon" "mood: sunny" "not a field"]))))
  (testing "a field with no value is left out"
    (is (= {} (header/fields ["title:" "summary:   "])))))
