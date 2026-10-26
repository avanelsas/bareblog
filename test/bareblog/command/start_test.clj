(ns bareblog.command.start-test
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [bareblog.blog.layout :as layout]
            [bareblog.blog.shape :as shape]
            [bareblog.blog.site :as site]
            [bareblog.command.start :as start]
            [bareblog.example :as example]
            [bareblog.publish.git :as git]
            [bareblog.publish.run :as run]
            [bareblog.read.source :as source]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(deftest draft-test
  (testing "a draft has a folder named for its title, and a post that starts with the title"
    (is (= {:folder "a-new-blog-post" :text "# A new blog post\n\n"} (start/first-text "A new blog post")))
    (is (= {:folder "cafe-lisboa-2026" :text "# Café Lisboa, 2026!\n\n"} (start/first-text " Café Lisboa, 2026! "))))
  (testing "the heading is the title as it was typed"
    (is (= {:folder "iphone-notes" :text "# iPhone notes\n\n"} (start/first-text "iPhone notes"))))
  (testing "a title that gives no name for a folder is a problem"
    (is (some? (:problem (start/first-text "!!!"))))
    (is (some? (:problem (start/first-text "Tags"))))
    (is (nil? (:folder (start/first-text "Archive"))))))

(deftest draft-in-a-blog-test
  (example/with-copy {}
    (fn [dir]
      (let [made (start/draft! dir "A third thought")]
        (testing "the draft is a folder in the drafts of the blog, with its post"
          (is (= (str (fs/path dir "drafts" "a-third-thought" "post.md")) (:file made)))
          (is (= "# A third thought\n\n" (slurp (:file made)))))
        (testing "a title of a post that is there already, as a draft or published, makes nothing"
          (is (re-find #"already" (:problem (start/draft! dir "A third thought"))))
          (is (re-find #"already" (:problem (start/draft! dir "A week in Lisbon"))))
          (is (= "# A third thought\n\n" (slurp (:file made)))))
        (testing "a title that gives no folder makes nothing"
          (is (some? (:problem (start/draft! dir "???")))))))))

;; The directory is one outside every repository, or a blog in it would be inside another.
(defn- with-empty-dir
  "What `check` gives for a directory with nothing in it, which must be outside every git
   repository for these tests to say anything."
  [check]
  (example/with-empty-dir
    (fn [dir]
      (is (not (git/inside? dir)) "The tests need a temporary directory outside every repository.")
      (check dir))))

(def all-ignored "drafts/\n.DS_Store\n._*\n")

(deftest ignored-test
  (testing "a new blog keeps its drafts and the files of a Mac out of git, each on its own line"
    (is (= all-ignored (start/with-ignored ""))))
  (testing "what is missing comes after what is there, also when that ends without a new line"
    (is (= ".DS_Store\ndrafts/\n._*\n" (start/with-ignored ".DS_Store\n")))
    (is (= (str "node_modules/\n" all-ignored) (start/with-ignored "node_modules/"))))
  (testing "a text that has them all, in any way that keeps out the same, is left as it is"
    (doseq [text [all-ignored
                  "._*\n.DS_Store\n/drafts/\n"
                  "drafts\n**/.DS_Store\n**/._*\nnode_modules/\n"
                  "  /drafts  \n.DS_Store\n._*"]]
      (is (= text (start/with-ignored text)))))
  (testing "a file of a Mac that is kept out at the top only is not kept out of a folder"
    (is (= "drafts/\n/.DS_Store\n._*\n.DS_Store\n"
           (start/with-ignored "drafts/\n/.DS_Store\n._*\n"))))
  (testing "a line that only holds the word, or that takes it back, is not that"
    (is (= (str "old-drafts/\n" all-ignored) (start/with-ignored "old-drafts/\n")))
    (is (= (str "!.DS_Store\n" all-ignored) (start/with-ignored "!.DS_Store\n"))))
  (testing "a text keeps the ends of its lines"
    (is (= "node_modules/\r\ndrafts/\r\n.DS_Store\r\n._*\r\n"
           (start/with-ignored "node_modules/\r\n")))))

(deftest next-steps-test
  (testing "an author is told how to write and see a post, and how to publish on GitHub Pages"
    (is (every? (partial str/includes? start/next-steps)
                ["bareblog new" "bareblog preview" "bareblog publish --all" "git push -u origin main"
                 "from /docs" "https://<you>.github.io/<repository>/" "set :author and :address"]))))

(deftest settings-text-test
  (testing "the settings of a new blog are settings: its title, and what else a blog needs"
    (is (= {:title "My \"first\" blog" :language "en" :template :journal :available-themes start/themes
            :per-page 10 :powered-by-footer true :gradient-titles false
            :navbar-colors false :page-colors false}
           (edn/read-string (start/settings-text " My \"first\" blog ")))))
  (testing "a new blog has every theme of BareDOM, each once, and starts with ocean"
    (is (= "ocean" (first start/themes)))
    (is (= #{"default" "ocean" "forest" "sunset" "neo-brutalist" "aurora" "mono-ai" "warm-mineral"}
           (set start/themes)))
    (is (apply distinct? start/themes)))
  (testing "every setting that a blog has a value for when it says nothing is written with that value"
    (is (= [site/default-language shape/default-template site/default-per-page
            site/default-powered-by-footer site/default-gradient-titles site/default-navbar-colors
            site/default-page-colors]
           ((juxt :language :template :per-page :powered-by-footer :gradient-titles :navbar-colors
                  :page-colors)
            (edn/read-string (start/settings-text "My blog"))))))
  (testing "they show where the author and the address go, name neither, and say when to set them"
    (is (re-find #";; :author \"Your name\"" (start/settings-text "My blog")))
    (is (re-find #";; :address \"https://<you>" (start/settings-text "My blog")))
    (is (re-find #"before you publish" (start/settings-text "My blog")))
    (is (not-any? (edn/read-string (start/settings-text "My blog")) [:author :address])))
  (testing "they show how a blog gets a font, and name none"
    (is (re-find #"bareblog font \"Literata\"" (start/settings-text "My blog")))
    (is (re-find #";; :font \"Literata\"" (start/settings-text "My blog")))
    (is (not (contains? (edn/read-string (start/settings-text "My blog")) :font)))))

(deftest readme-text-test
  (testing "the README names the folders and the files of a blog as the blog has them"
    (is (every? #(str/includes? start/readme-text (str "`" % "/`"))
                (conj layout/written-dirs layout/output-dir)))
    (is (every? #(str/includes? start/readme-text (str "`" % "`"))
                (conj layout/text-files layout/settings-file layout/series-file)))))

(deftest init-test
  (with-empty-dir
    (fn [dir]
      (let [made (start/blog! dir "My blog")]
        (testing "a new blog has its settings, its folders, and a repository with what it keeps out"
          (is (nil? (:problem made)))
          (is (source/blog? dir))
          (is (= "My blog" (:title (source/read-settings dir))))
          (is (every? #(fs/directory? (fs/path dir %)) ["posts" "drafts" "pages"]))
          (is (= all-ignored (slurp (fs/file dir ".gitignore"))))
          (is (git/top? dir))
          (is (= "It is a git repository now." (:said made))))
        (testing "a new blog has a README of how BareBlog writes and publishes it"
          (is (= start/readme-text (slurp (fs/file dir "README.md")))))
        (testing "a directory that is a blog already is left as it is"
          (spit (fs/file dir "blog.edn") "{:title \"Mine\"}")
          (is (re-find #"already" (:problem (start/blog! dir "Another"))))
          (is (= "{:title \"Mine\"}" (slurp (fs/file dir "blog.edn"))))))))
  (testing "a directory that holds other things, and is no repository, does not become one"
    (with-empty-dir
      (fn [dir]
        (spit (fs/file dir "notes.txt") "Mine.")
        (let [made (start/blog! dir "My blog")]
          (is (re-find #"holds other things" (:problem made)))
          (is (= ["notes.txt"] (map fs/file-name (fs/list-dir dir))))))))
  (testing "a repository that holds other things becomes a blog, and its README and what git
            keeps out stay"
    (with-empty-dir
      (fn [dir]
        (git/init! dir)
        (spit (fs/file dir "README.md") "# Mine")
        (spit (fs/file dir ".gitignore") ".DS_Store\n")
        (is (= "It is a git repository already. Its README.md was there already, and is left as it is."
               (:said (start/blog! dir "My blog"))))
        (is (= "# Mine" (slurp (fs/file dir "README.md"))))
        (is (source/blog? dir))
        (is (= ".DS_Store\ndrafts/\n._*\n" (slurp (fs/file dir ".gitignore")))))))
  (testing "a directory inside another repository gets none of its own, and the author is told"
    (with-empty-dir
      (fn [dir]
        (git/init! dir)
        (let [inside (str (fs/create-dirs (fs/path dir "a-blog")))
              made   (start/blog! inside "My blog")]
          (is (source/blog? inside))
          (is (not (git/top? inside)))
          (is (re-find #"inside another git repository" (:said made))))))))

;; The whole path of an author, but for the remote: a blog is made, a post is written and the
;; blog is published. The saver writes the address it was given, and the app is one file.
(deftest whole-path-test
  (with-empty-dir
    (fn [top]
      (let [dir  (str (fs/create-dirs (fs/path top "my-blog")))
            app  (str (fs/create-dirs (fs/path top "the-app")))
            said (atom [])]
        (spit (fs/file app "index.html") "the page of the app")
        (start/blog! dir "My blog")
        (process/shell {:dir dir} "git" "config" "user.name" "A test")
        (process/shell {:dir dir} "git" "config" "user.email" "test@example.com")
        (process/shell {:dir dir} "git" "config" "commit.gpgsign" "false")
        (spit (:file (start/draft! dir "A first post")) "# A first post\n\nIt has a paragraph.\n")
        (let [came (run/publish! {:save (fn [pages saved!]
                                              (doseq [{:keys [address file]} pages]
                                                (fs/create-dirs (fs/parent file))
                                                (spit (fs/file file) address)
                                                (saved! file)))
                                      :say  (partial swap! said conj)
                                      :ask  (constantly true)
                                      :app  app}
                                     dir
                                     {:folder "a-first-post" :now "2026-10-12T09:00:00+02:00"})]
          (testing "the post is published and committed, and only the push has nowhere to go"
            (is (= :not-pushed (:outcome came)))
            (is (= "Publish a-first-post" (:message came)))
            (is (fs/regular-file? (fs/path dir "docs" "a-first-post" "index.html")))
            (is (fs/regular-file? (fs/path dir "posts" "a-first-post" "post.md")))
            (is (= [{:folder "a-first-post" :title "A first post" :date "2026-10-12T09:00:00+02:00"}]
                   (map #(select-keys % [:folder :title :date])
                        (:posts (edn/read-string (slurp (fs/file dir "docs" "site.edn")))))))))))))

(deftest page-test
  (example/with-copy {}
    (fn [dir]
      (let [made (start/page! dir "Contact me")]
        (testing "a new page is a folder among the pages, with its text"
          (is (= (str (fs/path dir "pages" "contact-me" "page.md")) (:file made)))
          (is (= "# Contact me\n\n" (slurp (:file made))))
          (is (some #{"contact-me"} (map :folder (:pages (source/read-site (source/preview dir)))))))
        (testing "a title that is taken, by a page, a post or a draft, makes nothing"
          (is (re-find #"already" (:problem (start/page! dir "About"))))
          (is (re-find #"already" (:problem (start/page! dir "A week in Lisbon"))))
          (is (re-find #"already" (:problem (start/draft! dir "Contact me")))))))))

(deftest git-init-test
  (testing "git says why when it cannot make a repository"
    (with-empty-dir
      (fn [dir]
        (spit (fs/file dir ".git") "not a repository")
        (is (string? (:problem (git/init! dir))))))))
