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
    (is (= [{:kind :unknown :type :todo-list :text "todo"}]
           (blocks "- [ ] todo"))))
  (testing "a post with no text has no blocks"
    (is (= [] (blocks "")))))

(deftest inline-kinds-test
  (testing "text that is struck"
    (is (= [{:kind :text :content ["no " {:kind :strikethrough :content ["more"]}]}]
           (blocks "no ~~more~~"))))
  (testing "a line break that the author asks for"
    (is (= [{:kind :text :content ["one" {:kind :break} "two"]}]
           (blocks "one  \ntwo"))))
  (testing "a comment in a sentence is nothing"
    (is (= [{:kind :text :content ["Before " " after."]}]
           (blocks "Before <!-- a note to myself --> after."))))
  (testing "HTML in a sentence is code, tag by tag"
    (is (= [{:kind :text :content ["Press " {:kind :code :content ["<kbd>"]} "Ctrl"
                                   {:kind :code :content ["</kbd>"]} "."]}]
           (blocks "Press <kbd>Ctrl</kbd>."))))
  (testing "a web address with nothing around it is a link"
    (is (= [{:kind :text :content [{:kind :link :href "https://example.com" :content ["https://example.com"]}]}]
           (blocks "https://example.com")))))

(deftest quote-test
  (testing "a quote holds blocks"
    (is (= [{:kind   :quote
             :blocks [{:kind :text :content ["One."]}
                      {:kind :text :content [{:kind :strong :content ["Two."]}]}]}]
           (blocks "> One.\n>\n> **Two.**")))))

(deftest list-test
  (testing "an item of a list holds blocks, and text with no blank line around it is plain"
    (is (= [{:kind  :list
             :items [{:blocks [{:kind :plain :content ["one"]}]}
                     {:blocks [{:kind :plain :content ["two"]}]}]}]
           (blocks "- one\n- two"))))
  (testing "a list in an item is a block of that item"
    (is (= [{:kind  :list
             :items [{:blocks [{:kind :plain :content ["one"]}
                               {:kind :list :items [{:blocks [{:kind :plain :content ["below"]}]}]}]}]}]
           (blocks "- one\n  - below"))))
  (testing "a numbered list knows where it starts"
    (is (= {:kind :list :numbered? true :start 3}
           (dissoc (first (blocks "3. three\n4. four")) :items))))
  (testing "items with a blank line between them hold paragraphs"
    (is (= [{:blocks [{:kind :text :content ["one"]}]} {:blocks [{:kind :text :content ["two"]}]}]
           (:items (first (blocks "1. one\n\n2. two")))))))

(deftest code-test
  (testing "a fence gives its language and its text"
    (is (= [{:kind :code :language "clojure" :text "(inc 1)\n"}]
           (blocks "```clojure\n(inc 1)\n```"))))
  (testing "a fence with no language has none"
    (is (= [{:kind :code :text "plain\n"}] (blocks "```\nplain\n```"))))
  (testing "a comment on lines of its own is nothing, also over more lines"
    (is (= [{:kind :text :content ["Before."]} {:kind :text :content ["After."]}]
           (blocks "Before.\n\n<!-- a note\n     to myself -->\n\nAfter."))))
  (testing "HTML on lines of its own is code in the language html"
    (is (= [{:kind :code :language "html" :text "<details>\n  <summary>More</summary>\n</details>"}]
           (blocks "<details>\n  <summary>More</summary>\n</details>")))))

(deftest divider-test
  (is (= [{:kind :text :content ["Above."]} {:kind :divider} {:kind :text :content ["Below."]}]
         (blocks "Above.\n\n---\n\nBelow."))))

(deftest table-test
  (testing "a table has a head and rows of cells, each with the side it is aligned to"
    (is (= [{:kind :table
             :head [{:content ["Name"] :align :left} {:content ["Size"] :align :right}]
             :rows [[{:content ["a"] :align :left} {:content ["1"] :align :right}]
                    [{:content [{:kind :emphasis :content ["b"]}] :align :left} {:content ["2"] :align :right}]]}]
           (blocks "| Name | Size |\n|:-----|-----:|\n| a | 1 |\n| *b* | 2 |"))))
  (testing "a column with no side has none"
    (is (= [{:content ["Name"]}] (:head (first (blocks "| Name |\n|------|\n| a |")))))))

(deftest footnotes-test
  (let [read (blocks "First[^b] and second[^a].\n\n[^a]: Note a.\n[^b]: Note **b**.")]
    (testing "a mark in the text has the number of its note, in the order of the marks"
      (is (= [{:kind :text :content ["First" {:kind :footnote-mark :number 1 :first? true}
                                     " and second" {:kind :footnote-mark :number 2 :first? true} "."]}]
             (butlast read))))
    (testing "the notes are the last block, in the order of their numbers, and hold blocks"
      (is (= {:kind  :footnotes
              :notes [{:number 1 :blocks [{:kind :text :content ["Note " {:kind :strong :content ["b"]} "."]}]}
                      {:number 2 :blocks [{:kind :text :content ["Note a."]}]}]}
             (last read)))))
  (testing "of the marks of one note, the first in the text is the first, wherever it stands"
    (is (= [[{:number 1 :first? true} {:number 2 :first? true} {:number 1}] [{:number 2}]]
           (let [[quoted later] (blocks "> A[^b] b[^a] c[^b].\n\nLater[^a].\n\n[^a]: One.\n[^b]: Two.")]
             [(mapv #(dissoc % :kind) (filter map? (:content (first (:blocks quoted)))))
              (mapv #(dissoc % :kind) (filter map? (:content later)))]))))
  (testing "a note that no mark uses is left out"
    (is (= [{:kind :text :content ["Text."]}] (blocks "Text.\n\n[^a]: Never used."))))
  (testing "a mark with no note is the text that was typed"
    (is (= [{:kind :text :content ["One[^zz]."]}] (blocks "One[^zz]."))))
  (testing "a post with no footnote has no block for them"
    (is (= [{:kind :text :content ["Text."]}] (blocks "Text.")))))

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
