(ns bareblog.post-test
  (:require [bareblog.post :as post]
            [clojure.test :refer [deftest is testing]]))

(def blocks [{:kind :text :content ["Text."]}])

(deftest from-test
  (testing "the title of the header block is the title"
    (is (= "From the header"
           (:title (post/from "a-post" {:fields {:title "From the header"} :heading "Other" :blocks blocks})))))
  (testing "with no title in the header block, the heading that opens the text is the title"
    (is (= "From the heading" (:title (post/from "a-post" {:fields {} :heading "From the heading" :blocks blocks})))))
  (testing "with neither, the name of the folder is the title"
    (is (= "a-post" (:title (post/from "a-post" {:fields {} :heading nil :blocks blocks})))))
  (testing "a post is its fields, its title, its summary and its blocks"
    (is (= {:date "2026-10-11" :tags ["travel"] :title "a-post" :summary "Text." :blocks blocks}
           (post/from "a-post" {:fields {:date "2026-10-11" :tags ["travel"]} :heading nil :blocks blocks})))))

(deftest summary-test
  (testing "the summary of the header block is the summary"
    (is (= "From the header"
           (:summary (post/from "a-post" {:fields {:summary "From the header"} :blocks blocks})))))
  (testing "with none in the header block, the first paragraph is the summary, as plain text"
    (is (= "We took the tram 28 up."
           (:summary (post/from "a-post"
                                {:fields {}
                                 :blocks [{:kind :heading :level 2 :content ["A heading"]}
                                          {:kind :photo :src "a.jpg" :alt "A photo"}
                                          {:kind    :text
                                           :content ["We took the " {:kind :strong :content ["tram 28"]}
                                                     " up" {:kind :footnote-mark :number 1} "."]}
                                          {:kind :text :content ["Later."]}]})))))
  (testing "a long first paragraph is cut after a whole word, and marked"
    (let [long-text (apply str (repeat 100 "word "))
          summary   (:summary (post/from "a-post" {:fields {} :blocks [{:kind :text :content [long-text]}]}))]
      (is (<= (count summary) (inc post/summary-length)))
      (is (re-matches #"(word )+word\u2026" summary))))
  (testing "a summary from the header block is never cut"
    (let [long-text (apply str (repeat 100 "word "))]
      (is (= long-text (:summary (post/from "a-post" {:fields {:summary long-text} :blocks blocks}))))))
  (testing "a post with no paragraph has no summary"
    (is (not (contains? (post/from "a-post" {:fields {} :blocks [{:kind :divider}]}) :summary)))))

(deftest problems-test
  (testing "a post keeps what is wrong with it"
    (is (= ["A problem."] (:problems (post/from "a-post" {:fields {} :problems ["A problem."] :blocks blocks})))))
  (testing "a post with nothing wrong has no problems"
    (is (not (contains? (post/from "a-post" {:fields {} :problems [] :blocks blocks}) :problems)))))

(deftest folder-test
  (testing "a name that looks like a folder is one"
    (is (post/folder? "a-week-in-lisbon")))
  (testing "a name that the blog keeps for an address of its own is none"
    (is (not (post/folder? "tags")))
    (is (not (post/folder? "archive")))
    (is (not (post/folder? "page")))
    (is (not (post/folder? "js"))))
  (testing "a name that does not look like a folder is none"
    (is (not (post/folder? "A Week")))
    (is (not (post/folder? ".git")))))

(def with-images
  {:title  "A post"
   :blocks [{:kind :photo :src "harbour.jpg" :alt "The harbour"}
            {:kind    :text
             :content ["See " {:kind :strong :content [{:kind :image :src "map.png" :alt "a map"}]}
                       " and " {:kind :link :href "notes.txt" :content ["notes"]}]}
            {:kind :photo :src "https://example.com/far.jpg" :alt "Far away"}
            {:kind :photo :src "../other-post/a.jpg" :alt "Next door"}
            {:kind :photo :src "sub/b.jpg" :alt "Below"}
            {:kind :photo :src "my%20photo.jpg" :alt "With a space"}]})

(deftest file-beside-test
  (testing "a name is the name of a file beside the post"
    (is (= "harbour.jpg" (post/file-beside "harbour.jpg")))
    (is (= "café.jpg" (post/file-beside "café.jpg")))
    (is (= "a+b.jpg" (post/file-beside "a+b.jpg"))))
  (testing "a percent escape is read as its character"
    (is (= "my photo.jpg" (post/file-beside "my%20photo.jpg")))
    (is (= "café.jpg" (post/file-beside "caf%C3%A9.jpg"))))
  (testing "an address that leads out of the folder names no file beside the post"
    (is (nil? (post/file-beside "../other-post/a.jpg")))
    (is (nil? (post/file-beside "%2e%2e/a.jpg")))
    (is (nil? (post/file-beside "sub/b.jpg")))
    (is (nil? (post/file-beside "/etc/passwd")))
    (is (nil? (post/file-beside "https://example.com/far.jpg"))))
  (testing "what an address asks of a file, and the place in it that it names, are not in the name"
    (is (= "a.jpg" (post/file-beside "a.jpg#top")))
    (is (= "a.jpg" (post/file-beside "a.jpg?size=2"))))
  (testing "a place in the page itself names no file"
    (is (nil? (post/file-beside "#note")))
    (is (nil? (post/file-beside "?page=2"))))
  (testing "a question mark that is spelled as an escape is part of the name"
    (is (= "what?.jpg" (post/file-beside "what%3F.jpg"))))
  (testing "the names that a page keeps for itself name no file"
    (is (nil? (post/file-beside "page.edn")))
    (is (nil? (post/file-beside "changes")))
    (is (nil? (post/file-beside "index.html"))))
  (testing "the name of a file on disk is held to the same rule, as it is written"
    (is (post/file-name? "harbour.jpg"))
    (is (post/file-name? "100%.jpg"))
    (is (not-any? post/file-name? [".DS_Store" "index.html" "page.edn" "a..b" "sub/b.jpg"])))
  (testing "a hidden file, nothing, and a broken escape name none"
    (is (nil? (post/file-beside ".env")))
    (is (nil? (post/file-beside "")))
    (is (nil? (post/file-beside nil)))
    (is (nil? (post/file-beside "a%zz.jpg")))))

(deftest sources-test
  (testing "the addresses of the files beside the post, at any depth, as they are written"
    (is (= #{"harbour.jpg" "map.png" "my%20photo.jpg"} (post/sources with-images))))
  (testing "a post with no photo has none"
    (is (= #{} (post/sources {:blocks [{:kind :text :content ["Text."]}]})))))

(deftest sized-test
  (let [sized (post/sized with-images {"harbour.jpg" {:width 300 :height 200}
                                       "map.png"     {:width 64 :height 64}})]
    (testing "a photo gets its width and height"
      (is (= {:kind :photo :src "harbour.jpg" :alt "The harbour" :width 300 :height 200}
             (first (:blocks sized)))))
    (testing "an image inside a block gets them too"
      (is (= {:kind :image :src "map.png" :alt "a map" :width 64 :height 64}
             (get-in sized [:blocks 1 :content 1 :content 0]))))
    (testing "a photo whose size is not known stays as it is"
      (is (= (nth (:blocks with-images) 2) (nth (:blocks sized) 2))))
    (testing "everything else stays as it is"
      (is (= (dissoc with-images :blocks) (dissoc sized :blocks)))
      (is (= with-images (post/sized with-images {}))))))

(deftest placed-test
  (let [placed (post/placed with-images "/a-post/")]
    (testing "a photo beside the post gets the address of its file"
      (is (= "/a-post/harbour.jpg" (get-in placed [:blocks 0 :src])))
      (is (= "/a-post/my%20photo.jpg" (get-in placed [:blocks 5 :src])))
      (is (= "/my-blog/a-post/harbour.jpg"
             (get-in (post/placed with-images "/my-blog/a-post/") [:blocks 0 :src]))))
    (testing "an image and a link inside a block get it too"
      (is (= "/a-post/map.png" (get-in placed [:blocks 1 :content 1 :content 0 :src])))
      (is (= "/a-post/notes.txt" (get-in placed [:blocks 1 :content 3 :href]))))
    (testing "an address that names no file beside the post stays as it is"
      (is (= (subvec (:blocks with-images) 2 5) (subvec (:blocks placed) 2 5)))
      (is (= {:kind :link :href "#note" :content ["a note"]}
             (first (:blocks (post/placed {:blocks [{:kind :link :href "#note" :content ["a note"]}]} "/a-post/"))))))
    (testing "everything else stays as it is"
      (is (= (dissoc with-images :blocks) (dissoc placed :blocks))))))

(deftest folder-rule-test
  (testing "an author is told what the name of a folder may be, with the names that are kept"
    (is (re-find #"small letters, digits and dashes" post/folder-rule))
    (is (re-find #"archive, js, page, tags" post/folder-rule))))

(deftest folder-for-test
  (testing "a title gives a name for a folder: its letters and digits, with dashes between"
    (is (= "a-week-in-lisbon" (post/folder-for "A week in Lisbon")))
    (is (= "2026-in-review" (post/folder-for "2026 in review")))
    (is (= "what-s-new" (post/folder-for "  What's new?! "))))
  (testing "an accent is taken off its letter"
    (is (= "cafe-lisboa" (post/folder-for "Café Lisboa"))))
  (testing "such a name is the name of a post folder"
    (is (every? (comp post/folder? post/folder-for) ["A week in Lisbon" "Café" "C++ in 2026"])))
  (testing "a title with no letter or digit gives no name"
    (is (= "" (post/folder-for "!!!")))
    (is (= "" (post/folder-for nil)))))

(deftest folder-pattern-test
  (testing "the name of a folder is small letters, digits and dashes, and does not start with a dash"
    (is (re-matches (re-pattern post/folder-pattern) "a-week-in-lisbon"))
    (is (re-matches (re-pattern post/folder-pattern) "2026-in-review"))
    (is (nil? (re-matches (re-pattern post/folder-pattern) "-draft")))
    (is (nil? (re-matches (re-pattern post/folder-pattern) "A Week")))
    (is (nil? (re-matches (re-pattern post/folder-pattern) "a/b")))))
