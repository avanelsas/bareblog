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
  (testing "a post is its fields, its title and its blocks"
    (is (= {:date "2026-10-11" :tags ["travel"] :title "a-post" :blocks blocks}
           (post/from "a-post" {:fields {:date "2026-10-11" :tags ["travel"]} :heading nil :blocks blocks})))))

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

(deftest folder-pattern-test
  (testing "the name of a folder is small letters, digits and dashes, and does not start with a dash"
    (is (re-matches (re-pattern post/folder-pattern) "a-week-in-lisbon"))
    (is (re-matches (re-pattern post/folder-pattern) "2026-in-review"))
    (is (nil? (re-matches (re-pattern post/folder-pattern) "-draft")))
    (is (nil? (re-matches (re-pattern post/folder-pattern) "A Week")))
    (is (nil? (re-matches (re-pattern post/folder-pattern) "a/b")))))
