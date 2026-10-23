(ns bareblog.read.reader-test
  (:require [bareblog.read.reader :as reader]
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
  (testing "a fence of HTML marked live is a live block with its text"
    (is (= [{:kind :live :text "<x-icon></x-icon>\n"}]
           (blocks "```html live\n<x-icon></x-icon>\n```"))))
  (testing "a live fence gives the height that it names in pixels"
    (is (= [{:kind :live :text "<x-icon></x-icon>\n" :height 300}]
           (blocks "```html live height=300\n<x-icon></x-icon>\n```")))
    (is (= 300 (:height (first (blocks "```html height=300px live\nx\n```"))))))
  (testing "a height that is no whole number of pixels is left out"
    (is (= [nil nil nil]
           (map (comp :height first blocks)
                ["```html live height=tall\nx\n```"
                 "```html live height=0\nx\n```"
                 "```html live height=12.5\nx\n```"]))))
  (testing "a live fence of another language stays code"
    (is (= [{:kind :code :language "js" :text "run()\n"}]
           (blocks "```js live\nrun()\n```"))))
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
  (testing "a heading of the first level that opens the text is the heading, and is not a block"
    (is (= {:heading "A title" :tags nil :blocks [{:kind :text :content ["Text."]}]}
           (reader/read-post "# A title\n\nText."))))
  (testing "the heading is plain text"
    (is (= "A bold title" (:heading (reader/read-post "# A **bold** title")))))
  (testing "a text that opens with something else has no heading"
    (is (nil? (:heading (reader/read-post "Text.\n\n# Later")))))
  (testing "a later heading of the first level stays a block"
    (is (= [{:kind :text :content ["Text."]} {:kind :heading :level 1 :content ["Again"]}]
           (blocks "Text.\n\n# Again")))))

(deftest tags-test
  (testing "a last paragraph of nothing but tags names the tags, and is no block"
    (is (= {:tags ["travel" "two words" "x-code"] :blocks [{:kind :text :content ["Text."]}]}
           (select-keys (reader/read-post "# T\n\nText.\n\n#travel #two_words #x-code\n") [:tags :blocks]))))
  (testing "the footnotes may come after it, and are still the last block"
    (let [read (reader/read-post "Text[^a].\n\n#travel\n\n[^a]: A note.")]
      (is (= ["travel"] (:tags read)))
      (is (= [:text :footnotes] (map :kind (:blocks read))))))
  (testing "a tag in a sentence, and a paragraph of tags that is not the last, stay text"
    (is (= {:tags nil :blocks [{:kind :text :content ["See #travel."]}]}
           (select-keys (reader/read-post "See #travel.") [:tags :blocks])))
    (is (= [{:kind :text :content ["#first"]} {:kind :text :content ["Then text."]}]
           (blocks "#first\n\nThen text."))))
  (testing "a last paragraph of tags with something marked in it stays text"
    (is (nil? (:tags (reader/read-post "Text.\n\n#travel **#bold**")))))
  (testing "underscores between tags that stand beside each other are no emphasis"
    (is (= ["a b" "c d" "two words"] (:tags (reader/read-post "Text.\n\n#a_b #c_d #two__words")))))
  (testing "a tag that starts with an underscore is emphasis to Markdown, and the line stays text"
    (is (= {:tags nil :blocks [{:kind :text :content ["Text."]}
                               {:kind :text :content ["#" {:kind :emphasis :content ["draft"]}]}]}
           (select-keys (reader/read-post "Text.\n\n#_draft_") [:tags :blocks]))))
  (testing "a block of fields between dashes is read as any text is, and names nothing"
    (let [read (reader/read-post "---\ntitle: Old\n---\n\nText.")]
      (is (nil? (:heading read)))
      (is (= :divider (:kind (first (:blocks read)))))))
  (testing "a text that is nothing but its tags has no blocks"
    (is (= {:tags ["travel"] :blocks []} (select-keys (reader/read-post "#travel") [:tags :blocks])))))

(deftest tags-of-a-paragraph-test
  (testing "a paragraph of nothing but tags names them, in the order they are written"
    (is (= ["travel" "portugal"] (reader/tags "#travel #portugal")))
    (is (= ["travel"] (reader/tags "  #travel  "))))
  (testing "an underscore is a space, and a dash is a dash"
    (is (= ["two words" "x-code" "slow trains in spring"]
           (reader/tags "#two_words #x-code #slow_trains_in_spring"))))
  (testing "underscores that follow each other are one space, and one at the end is nothing"
    (is (= ["two words" "a"] (reader/tags "#two__words #a_"))))
  (testing "a tag may hold what is no letter, and keeps its capitals"
    (is (= ["c++" "100%" "Café" "v1.2" "1"] (reader/tags "#c++ #100% #Café #v1.2 #1"))))
  (testing "the tags may stand on more lines than one, and a tag that is named twice is one tag"
    (is (= ["a" "b"] (reader/tags "#a\n#b #a"))))
  (testing "a paragraph that holds anything else names no tags"
    (is (nil? (reader/tags "See #travel for more.")))
    (is (nil? (reader/tags "#travel and more")))
    (is (nil? (reader/tags "#")))
    (is (nil? (reader/tags "##x-code")))
    (is (nil? (reader/tags ""))))
  (testing "a tag does not start with an underscore"
    (is (nil? (reader/tags "#_draft")))
    (is (nil? (reader/tags "#a #_b")))))

(deftest tag-names-test
  (testing "a tag is written with its mark, and a space as an underscore"
    (is (= "#web_components" (reader/written-tag "web components")))
    (is (= "#x-code" (reader/written-tag "x-code"))))
  (testing "what an author names a tag is read as a tag is read"
    (is (= "web components" (reader/tag-named " web_components ")))
    (is (= "a b" (reader/tag-named "a__b"))))
  (testing "a tag can be written when it is read back as itself"
    (is (every? reader/tag? ["travel" "web components" "c++" "BareDOM"]))
    (is (not-any? reader/tag? ["" "a#b" "two#"]))))

(deftest retagged-test
  (let [text "# T\n\nText[^a] with #inline.\n\n#travel #two_words  #baredom\n\n[^a]: A note.\n"]
    (testing "the paragraph of tags is written with the new tags, and the rest of the text stays"
      (is (= "# T\n\nText[^a] with #inline.\n\n#travel #two_words #BareDOM\n\n[^a]: A note.\n"
             (reader/retagged text ["travel" "two words" "BareDOM"]))))
    (testing "a paragraph of tags over more lines becomes one line"
      (is (= "Text.\n\n#a #c\n" (reader/retagged "Text.\n\n#a\n#b\n" ["a" "c"]))))
    (testing "a text that names a tag twice is retagged with each tag once"
      (is (= "Text.\n\n#a #b\n" (reader/retagged "Text.\n\n#a #b #a\n" ["a" "b"]))))
    (testing "the line of tags ends as the lines of a text with a carriage return do"
      (is (= "# T\r\n\r\nText.\r\n\r\n#a #c\r\n" (reader/retagged "# T\r\n\r\nText.\r\n\r\n#a #b\r\n" ["a" "c"]))))
    (testing "a text with no paragraph of tags is not retagged"
      (is (nil? (reader/retagged "Text with #travel.\n" ["travel"]))))
    (testing "tags that would be read as other tags are not written"
      (is (nil? (reader/retagged text ["travel" "a#b"]))))))
