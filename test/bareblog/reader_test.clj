(ns bareblog.reader-test
  (:require [bareblog.reader :as reader]
            [clojure.test :refer [deftest is testing]]))

(defn- blocks [text]
  (:blocks (reader/read-post text)))

(deftest blocks-test
  (testing "a paragraph is a text block, and what marks its text is content with a kind"
    (is (= [{:kind    :text
             :content ["We took the "
                       {:kind :strong :content ["tram 28"]}
                       " up to "
                       {:kind :link :href "https://example.com" :content ["Alfama"]}
                       ", "
                       {:kind :emphasis :content ["slowly"]}
                       ", with "
                       {:kind :code :content ["bb"]}
                       "."]}]
           (blocks (str "We took the **tram 28** up to [Alfama](https://example.com), "
                        "*slowly*, with `bb`.")))))
  (testing "a link keeps its title"
    (is (= [{:kind    :text
             :content [{:kind :link :href "https://example.com" :title "A hill" :content ["Alfama"]}]}]
           (blocks "[Alfama](https://example.com \"A hill\")"))))
  (testing "content holds content"
    (is (= [{:kind :text :content [{:kind :strong :content ["very " {:kind :emphasis :content ["slow"]}]}]}]
           (blocks "**very *slow***"))))
  (testing "a line break inside a paragraph is a space"
    (is (= [{:kind :text :content ["one" " " "two"]}]
           (blocks "one\ntwo"))))
  (testing "a heading keeps its level"
    (is (= [{:kind :heading :level 2 :content ["The " {:kind :emphasis :content ["old"]} " town"]}]
           (blocks "## The *old* town"))))
  (testing "a paragraph that holds one image and nothing else is a photo block"
    (is (= [{:kind :photo :src "harbour.svg" :alt "The harbour at dawn"}]
           (blocks "![The harbour at dawn](harbour.svg)"))))
  (testing "an image beside text stays in its text block"
    (is (= [{:kind :text :content ["See " {:kind :image :src "a.png" :alt "a map"} " here."]}]
           (blocks "See ![a map](a.png) here."))))
  (testing "a node that BareBlog has no kind for yet says what it is, with its plain text"
    (is (= [{:kind :unknown :type :blockquote :text "A quote."}]
           (blocks "> A quote."))))
  (testing "a post with no text has no blocks"
    (is (= [] (blocks "")))))

(deftest read-post-test
  (testing "the fields of the header block are read"
    (is (= {:date "2026-10-11" :tags ["travel" "portugal"]}
           (:fields (reader/read-post "---\ndate: 2026-10-11\ntags: travel, portugal\n---\n\nText.")))))
  (testing "a heading of the first level that opens the text is the heading, and is not a block"
    (is (= {:fields {} :heading "A title" :blocks [{:kind :text :content ["Text."]}]}
           (reader/read-post "# A title\n\nText."))))
  (testing "the heading is plain text"
    (is (= "A bold title" (:heading (reader/read-post "# A **bold** title")))))
  (testing "a text that opens with something else has no heading"
    (is (nil? (:heading (reader/read-post "Text.\n\n# Later")))))
  (testing "a later heading of the first level stays a block"
    (is (= [{:kind :text :content ["Text."]} {:kind :heading :level 1 :content ["Again"]}]
           (blocks "Text.\n\n# Again")))))
