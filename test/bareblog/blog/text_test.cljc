(ns bareblog.blog.text-test
  (:require [bareblog.blog.text :as text]
            [clojure.test :refer [deftest is testing]]))

(def blocks [{:kind :text :content ["Text."]}])

(deftest from-test
  (testing "the heading that opens the text is the title"
    (is (= "From the heading" (:title (text/from "a-post" {:heading "From the heading" :blocks blocks})))))
  (testing "with no heading, the title is from the name of the folder"
    (is (= "A post" (:title (text/from "a-post" {:heading nil :blocks blocks})))))
  (testing "a text is its title, its tags, its summary and its blocks"
    (is (= {:title "A post" :tags ["travel"] :summary "Text." :blocks blocks}
           (text/from "a-post" {:heading nil :tags ["travel"] :blocks blocks}))))
  (testing "a text with no tags says nothing of tags"
    (is (= #{:title :summary :blocks} (set (keys (text/from "a-post" {:tags [] :blocks blocks})))))))

(deftest title-for-test
  (testing "the words of the folder with a space between them, and a capital for the first"
    (is (= "About" (text/title-for "about")))
    (is (= "A first post" (text/title-for "a-first-post")))
    (is (= "2026 in review" (text/title-for "2026-in-review")))))

(deftest summary-test
  (testing "the first paragraph is the summary, as plain text"
    (is (= "We took the tram 28 up."
           (:summary (text/from "a-post"
                                {:blocks [{:kind :heading :level 2 :content ["A heading"]}
                                          {:kind :photo :src "a.jpg" :alt "A photo"}
                                          {:kind    :text
                                           :content ["We took the " {:kind :strong :content ["tram 28"]}
                                                     " up" {:kind :footnote-mark :number 1} "."]}
                                          {:kind :text :content ["Later."]}]})))))
  (testing "a break of a line in the first paragraph is a space in the summary"
    (is (= "A line can break here."
           (:summary (text/from "a-post" {:blocks [{:kind :text :content ["A line can break" {:kind :break} "here."]}]})))))
  (testing "a long first paragraph is cut after a whole word, and marked"
    (let [long-text (apply str (repeat 100 "word "))
          summary   (:summary (text/from "a-post" {:blocks [{:kind :text :content [long-text]}]}))]
      (is (<= (count summary) (inc text/summary-length)))
      (is (re-matches #"(word )+word\u2026" summary)))))

(deftest folder-test
  (testing "a name that looks like a folder is one"
    (is (text/folder? "a-week-in-lisbon")))
  (testing "a name that the blog keeps for an address of its own is none"
    (is (not (text/folder? "tags")))
    (is (not (text/folder? "archive")))
    (is (not (text/folder? "page")))
    (is (not (text/folder? "js"))))
  (testing "a name that does not look like a folder is none"
    (is (not (text/folder? "A Week")))
    (is (not (text/folder? ".git")))))

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
    (is (= "harbour.jpg" (text/file-beside "harbour.jpg")))
    (is (= "café.jpg" (text/file-beside "café.jpg")))
    (is (= "a+b.jpg" (text/file-beside "a+b.jpg"))))
  (testing "a name that is written from where the post is, is the same name"
    (is (= "harbour.jpg" (text/file-beside "./harbour.jpg")))
    (is (= "harbour.jpg" (text/file-beside "././harbour.jpg")))
    (is (= "my photo.jpg" (text/file-beside "./my%20photo.jpg"))))
  (testing "a `./` does not let an address lead elsewhere, and is no name by itself"
    (is (nil? (text/file-beside "./sub/b.jpg")))
    (is (nil? (text/file-beside "./../a.jpg")))
    (is (nil? (text/file-beside "./")))
    (is (nil? (text/file-beside "./.env"))))
  (testing "a percent escape is read as its character"
    (is (= "my photo.jpg" (text/file-beside "my%20photo.jpg")))
    (is (= "café.jpg" (text/file-beside "caf%C3%A9.jpg"))))
  (testing "an address that leads out of the folder names no file beside the post"
    (is (nil? (text/file-beside "../other-post/a.jpg")))
    (is (nil? (text/file-beside "%2e%2e/a.jpg")))
    (is (nil? (text/file-beside "sub/b.jpg")))
    (is (nil? (text/file-beside "/etc/passwd")))
    (is (nil? (text/file-beside "https://example.com/far.jpg"))))
  (testing "what an address asks of a file, and the place in it that it names, are not in the name"
    (is (= "a.jpg" (text/file-beside "a.jpg#top")))
    (is (= "a.jpg" (text/file-beside "a.jpg?size=2"))))
  (testing "a place in the page itself names no file"
    (is (nil? (text/file-beside "#note")))
    (is (nil? (text/file-beside "?page=2"))))
  (testing "a question mark that is spelled as an escape is part of the name"
    (is (= "what?.jpg" (text/file-beside "what%3F.jpg"))))
  (testing "the names that a page keeps for itself name no file"
    (is (nil? (text/file-beside "page.edn")))
    (is (nil? (text/file-beside "changes")))
    (is (nil? (text/file-beside "index.html"))))
  (testing "the name of a file on disk is held to the same rule, as it is written"
    (is (text/file-name? "harbour.jpg"))
    (is (text/file-name? "100%.jpg"))
    (is (not-any? text/file-name? [".DS_Store" "index.html" "page.edn" "a..b" "sub/b.jpg"])))
  (testing "a hidden file, nothing, and a broken escape name none"
    (is (nil? (text/file-beside ".env")))
    (is (nil? (text/file-beside "")))
    (is (nil? (text/file-beside nil)))
    (is (nil? (text/file-beside "a%zz.jpg")))))

(deftest sources-test
  (testing "the addresses of the files beside the post, at any depth, as they are written"
    (is (= #{"harbour.jpg" "map.png" "my%20photo.jpg"} (text/sources with-images))))
  (testing "a post with no photo has none"
    (is (= #{} (text/sources {:blocks [{:kind :text :content ["Text."]}]})))))

(deftest dotted-sources-test
  (testing "a photo that is named from where the post is, is one of its files, and gets its size"
    (let [post {:blocks [{:kind :photo :src "./harbour.jpg" :alt "The harbour"}]}]
      (is (= #{"./harbour.jpg"} (text/sources post)))
      (is (= {:width 300 :height 200}
             (select-keys (first (:blocks (text/sized post {"./harbour.jpg" {:width 300 :height 200}})))
                          [:width :height]))))))

(deftest files-test
  (testing "the files beside a post that it shows or links to, by their names"
    (is (= #{"harbour.jpg" "map.png" "notes.txt" "my photo.jpg"} (text/files with-images))))
  (testing "they are the same once the post is placed, for the address it is placed under"
    (is (= (text/files with-images)
           (text/files (text/placed with-images "/my-blog/a-post/") "/my-blog/a-post/"))))
  (testing "what a file is asked for, and a place in it, are no part of its name"
    (is (= #{"notes.txt"}
           (text/files {:blocks [{:kind :link :href "./notes.txt?x=1#top" :content ["notes"]}]}))))
  (testing "a placed post that names a file under another address shows none of its own"
    (is (= #{} (text/files {:blocks [{:kind :link :href "/other-post/a.jpg" :content ["a"]}
                                     {:kind :photo :src "https://example.com/far.jpg"}
                                     {:kind :text :content ["Text."]}]}
                           "/a-post/")))))

(deftest split-cover-test
  (let [photo  {:kind :photo :src "harbour.jpg" :alt "The harbour"}
        boats  (assoc photo :src "boats.jpg")
        text   {:kind :text :content ["Text."]}
        inline {:kind :text :content ["See " {:kind :image :src "map.png"}]}]
    (testing "a photo that opens a post is its cover, and the blocks after it are the rest"
      (is (= {:cover photo :blocks [text]} (text/split-cover {:blocks [photo text]})))
      (is (= {:cover photo :blocks []} (text/split-cover {:blocks [photo]}))))
    (testing "of two photos that open a post, the first is its cover"
      (is (= {:cover photo :blocks [boats text]} (text/split-cover {:blocks [photo boats text]}))))
    (testing "a post that opens with anything else has no cover, and keeps all its blocks"
      (is (= {:cover nil :blocks [text photo]} (text/split-cover {:blocks [text photo]})))
      (is (= {:cover nil :blocks [inline]} (text/split-cover {:blocks [inline]})))
      (is (= {:cover nil :blocks []} (text/split-cover {:blocks []}))))))

(deftest pictures-test
  (testing "the pictures of a post are its photos and the images inside its blocks, in order"
    (is (= ["harbour.jpg" "map.png"] (take 2 (map :src (text/pictures with-images))))))
  (testing "a post with no picture has none"
    (is (empty? (text/pictures {:blocks [{:kind :text :content ["Text."]}]})))))

(deftest sized-test
  (let [sized (text/sized with-images {"harbour.jpg" {:width 300 :height 200}
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
      (is (= with-images (text/sized with-images {}))))))

(deftest placed-test
  (let [placed (text/placed with-images "/a-post/")]
    (testing "a photo beside the post gets the address of its file"
      (is (= "/a-post/harbour.jpg" (get-in placed [:blocks 0 :src])))
      (is (= "/a-post/my%20photo.jpg" (get-in placed [:blocks 5 :src])))
      (is (= "/my-blog/a-post/harbour.jpg"
             (get-in (text/placed with-images "/my-blog/a-post/") [:blocks 0 :src]))))
    (testing "an image and a link inside a block get it too"
      (is (= "/a-post/map.png" (get-in placed [:blocks 1 :content 1 :content 0 :src])))
      (is (= "/a-post/notes.txt" (get-in placed [:blocks 1 :content 3 :href]))))
    (testing "a file that is named from where the post is gets the same address, without the `./`"
      (let [dotted (text/placed {:blocks [{:kind :photo :src "./harbour.jpg"}
                                          {:kind :link :href "./notes.txt#top" :content ["notes"]}]}
                                "/a-post/")]
        (is (= "/a-post/harbour.jpg" (get-in dotted [:blocks 0 :src])))
        (is (= "/a-post/notes.txt#top" (get-in dotted [:blocks 1 :href])))))
    (testing "an address that names no file beside the post stays as it is"
      (is (= (subvec (:blocks with-images) 2 5) (subvec (:blocks placed) 2 5)))
      (is (= {:kind :link :href "#note" :content ["a note"]}
             (first (:blocks (text/placed {:blocks [{:kind :link :href "#note" :content ["a note"]}]} "/a-post/"))))))
    (testing "everything else stays as it is"
      (is (= (dissoc with-images :blocks) (dissoc placed :blocks))))))

(deftest folder-rule-test
  (testing "an author is told what the name of a folder may be, with the names that are kept"
    (is (re-find #"^A folder has small letters, digits and dashes" text/folder-rule))
    (is (re-find #"archive, fonts, js, page, tags" text/folder-rule))))

(deftest folder-for-test
  (testing "a title gives a name for a folder: its letters and digits, with dashes between"
    (is (= "a-week-in-lisbon" (text/folder-for "A week in Lisbon")))
    (is (= "2026-in-review" (text/folder-for "2026 in review")))
    (is (= "what-s-new" (text/folder-for "  What's new?! "))))
  (testing "an accent is taken off its letter"
    (is (= "cafe-lisboa" (text/folder-for "Café Lisboa"))))
  (testing "such a name is the name of a post folder"
    (is (every? (comp text/folder? text/folder-for) ["A week in Lisbon" "Café" "C++ in 2026"])))
  (testing "a title with no letter or digit gives no name"
    (is (= "" (text/folder-for "!!!")))
    (is (= "" (text/folder-for nil)))))

(deftest folder-pattern-test
  (testing "the name of a folder is small letters, digits and dashes, and does not start with a dash"
    (is (re-matches (re-pattern text/folder-pattern) "a-week-in-lisbon"))
    (is (re-matches (re-pattern text/folder-pattern) "2026-in-review"))
    (is (nil? (re-matches (re-pattern text/folder-pattern) "-draft")))
    (is (nil? (re-matches (re-pattern text/folder-pattern) "A Week")))
    (is (nil? (re-matches (re-pattern text/folder-pattern) "a/b")))))
