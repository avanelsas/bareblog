(ns bareblog.publish.git-test
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [bareblog.example :as example]
            [bareblog.publish.git :as git]
            [clojure.test :refer [deftest is testing]]))

(defn- with-repo
  "What `check` gives for a new directory that is a git repository."
  [check]
  (example/with-empty-dir
    (fn [dir]
      (git/init! dir)
      (doseq [[k v] example/git-settings]
        (process/shell {:dir dir :out :string} "git" "config" k v))
      (check dir))))

(def docs {:path "docs" :pattern "docs/" :source ".gitignore"})

(deftest list-ignored-test
  (with-repo
    (fn [dir]
      (spit (fs/file dir ".gitignore") "docs/\ndrafts/\n")
      (spit (fs/file dir "blog.edn") "{}")
      (fs/create-dirs (fs/path dir "docs"))
      (testing "a path that git keeps out is named with the line and the file that do so"
        (is (= [docs] (git/list-ignored dir ["blog.edn" "docs" "posts/a"]))))
      (testing "a directory that is not there yet is one too"
        (is (= #{"drafts/a" "docs"} (set (map :path (git/list-ignored dir ["drafts/a" "docs"]))))))
      (testing "with nothing kept out, and with no paths, there are none"
        (is (= [] (git/list-ignored dir ["blog.edn" "posts/a"])))
        (is (= [] (git/list-ignored dir []))))
      (testing "a line that keeps out directories does not keep out a file of that name"
        (spit (fs/file dir ".gitignore") "blog.edn/\n")
        (is (= [] (git/list-ignored dir ["blog.edn"]))))
      (testing "a path that a later line lets in again is not kept out"
        (spit (fs/file dir ".gitignore") "*.png\n!posts/a/*.png\n")
        (fs/create-dirs (fs/path dir "posts" "a"))
        (spit (fs/file dir "posts" "a" "p.png") "a picture")
        (spit (fs/file dir "q.png") "another")
        (is (= ["q.png"] (map :path (git/list-ignored dir ["posts/a/p.png" "q.png"])))))
      (testing "a line in another file of git is named by that file"
        (spit (fs/file dir ".gitignore") "")
        (spit (fs/file dir ".git" "info" "exclude") "docs/\n")
        (is (= [(assoc docs :source ".git/info/exclude")] (git/list-ignored dir ["docs"])))))))

(deftest list-ignored-files-test
  (with-repo
    (fn [dir]
      (spit (fs/file dir ".gitignore") "*.jpg\n.DS_Store\n")
      (fs/create-dirs (fs/path dir "docs" "a b"))
      (spit (fs/file dir "docs" "a b" "index.html") "a page")
      (spit (fs/file dir "docs" "a b" "harbour café.jpg") "a photo")
      (spit (fs/file dir "docs" ".DS_Store") "of a Mac")
      (testing "the files below a path that git keeps out are named, each with what keeps it out"
        (is (= #{{:path "docs/a b/harbour café.jpg" :pattern "*.jpg" :source ".gitignore"}
                 {:path "docs/.DS_Store" :pattern ".DS_Store" :source ".gitignore"}}
               (set (git/list-ignored-files dir ["docs"])))))
      (testing "below a path with none, and below one that is not there, there are none"
        (is (= [] (git/list-ignored-files dir ["posts"])))
        (spit (fs/file dir ".gitignore") "")
        (is (= [] (git/list-ignored-files dir ["docs"])))))))

(deftest commit-test
  (with-repo
    (fn [dir]
      (spit (fs/file dir ".gitignore") "docs/\n")
      (fs/create-dirs (fs/path dir "docs"))
      (spit (fs/file dir "docs" "index.html") "a page")
      (testing "what git says of a path it keeps out names the path, and holds no hint"
        (let [{:keys [problem]} (git/commit! dir ["docs"] "Publish")]
          (is (re-find #"ignored by one of your .gitignore files: docs$" problem))
          (is (not (re-find #"hint" problem))))))))
