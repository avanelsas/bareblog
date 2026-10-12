(ns bareblog.publish-test
  "A publish, from the folders to the remote. The blog is a copy of the example blog in a git
   repository of its own, the remote is a bare repository beside it, and the saver writes the
   address it was given."
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [bareblog.example :as example]
            [bareblog.git :as git]
            [bareblog.publish :as publish]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def now
  "The moment of every publish in these tests."
  "2026-10-12T09:00:00+02:00")

(defn- sh
  "What git says to `args` in `dir`, trimmed. Throws when git could not."
  [dir & args]
  (str/trim (:out (apply process/shell {:dir (str dir) :out :string :err :string} "git" args))))

(defn- with-repo
  "What `check` gives for the directory of a copy of the example blog that is a git repository
   with one commit, a remote in a bare repository, and an app of one file."
  [check]
  (example/with-copy {:address "https://avanelsas.github.io/my-blog/"}
    (fn [top]
      (let [dir    (str (fs/create-dirs (fs/path top "blog")))
            remote (str (fs/path top "remote.git"))
            app    (str (fs/create-dirs (fs/path top "the-app")))]
        (doseq [name ["blog.edn" "posts" "drafts" "pages"]]
          (fs/move (fs/path top name) (fs/path dir name)))
        (spit (fs/file app "index.html") "the page of the app")
        (spit (fs/file app "app.css") "the styles")
        (spit (fs/file dir ".gitignore") "drafts/\n")
        (sh top "init" "--quiet" "--bare" "-b" "main" remote)
        (sh dir "init" "--quiet" "-b" "main")
        (sh dir "config" "user.name" "A test")
        (sh dir "config" "user.email" "test@example.com")
        (sh dir "config" "commit.gpgsign" "false")
        (sh dir "remote" "add" "origin" remote)
        (sh dir "add" "--all")
        (sh dir "commit" "--quiet" "-m" "A blog")
        (sh dir "push" "--quiet" "-u" "origin" "main")
        (check {:dir dir :remote remote :app app})))))

;; The port of the server differs from run to run, so it is left out of what is saved. A page
;; that did not change is then the same file.
(defn- save-path [pages saved!]
  (doseq [{:keys [address file]} pages]
    (fs/create-dirs (fs/parent file))
    (spit (fs/file file) (str/replace address #"^http://localhost:\d+" ""))
    (saved! file)))

(defn- publish!
  "Publishes `request` of the blog in `dir`. `answer` gives the answer to a question: true or
   false for the same to each, or a function of the question. Gives what came of it, with what
   was said and what was asked."
  [{:keys [dir app]} request answer]
  (let [said  (atom [])
        asked (atom [])
        came  (publish/publish! {:save save-path
                                 :say  (partial swap! said conj)
                                 :ask  (fn [question]
                                         (swap! asked conj question)
                                         (if (fn? answer) (answer question) answer))
                                 :app  app}
                                dir
                                (assoc request :now now))]
    (assoc came :said @said :asked @asked)))

(defn- site [dir]
  (edn/read-string (slurp (fs/file dir "docs" "site.edn"))))

(defn- folders [dir]
  (mapv :folder (:posts (site dir))))

(defn- committed
  "The message of the last commit in `dir`, and the paths that it changed."
  [dir]
  {:message (sh dir "log" "-1" "--format=%s")
   :paths   (set (str/split-lines (sh dir "show" "--name-only" "--format=" "HEAD")))})

(defn- pushed? [{:keys [dir remote]}]
  (= (sh dir "rev-parse" "HEAD") (sh remote "rev-parse" "main")))

(defn- clean?
  "True when nothing in `dir` waits to be committed, but for what the test itself left there."
  [dir]
  (str/blank? (sh dir "status" "--porcelain")))

(defn- listed
  "The entry of the post in `folder` in the published list of the blog in `dir`."
  [dir folder]
  (first (filter (comp #{folder} :folder) (:posts (site dir)))))

(defn- head [dir]
  (sh dir "rev-parse" "HEAD"))

(deftest whole-blog-test
  (with-repo
    (fn [{:keys [dir] :as repo}]
      (let [{:keys [outcome message said]} (publish! repo {:all? true} true)]
        (testing "the whole blog is written, committed and pushed"
          (is (= [:published "Publish the whole blog"] [outcome message]))
          (is (= #{"every-kind" "a-week-in-lisbon" "why-web-components" "hello-bareblog"} (set (folders dir))))
          (is (= "/my-blog/archive/" (slurp (fs/file dir "docs" "archive" "index.html"))))
          (is (= "Publish the whole blog" (:message (committed dir))))
          (is (contains? (:paths (committed dir)) "docs/a-week-in-lisbon/index.html"))
          (is (pushed? repo))
          (is (clean? dir))
          (is (= "Published: Publish the whole blog." (last said))))
        (testing "a post with no date in its text gets the day it was published"
          (is (= now (:date (listed dir "hello-bareblog"))))
          (is (= now
                 (get-in (edn/read-string (slurp (fs/file dir "docs" "hello-bareblog" "page.edn"))) [:text :date]))))
        (testing "a draft is not published, and not committed"
          (is (not (fs/exists? (fs/path dir "docs" "a-new-idea"))))
          (is (fs/exists? (fs/path dir "drafts" "a-new-idea" "post.md"))))
        (testing "publishing again changes nothing, and says so"
          (let [before (head dir)
                again  (publish! repo {:all? true} true)]
            (is (= :unchanged (:outcome again)))
            (is (publish/done? (:outcome again)))
            (is (= before (head dir)))
            (is (= "Nothing changed, so nothing was committed." (last (:said again))))))))))

(deftest one-post-test
  (with-repo
    (fn [{:keys [dir] :as repo}]
      (publish! repo {:all? true} true)
      (testing "a draft that is published moves to the posts, and is in the list"
        (is (= :published (:outcome (publish! repo {:folder "a-new-idea"} true))))
        (is (fs/exists? (fs/path dir "posts" "a-new-idea" "post.md")))
        (is (not (fs/exists? (fs/path dir "drafts" "a-new-idea"))))
        (is (some #{"a-new-idea"} (folders dir)))
        (is (= "Publish a-new-idea" (:message (committed dir))))
        (is (contains? (:paths (committed dir)) "posts/a-new-idea/post.md"))
        (is (pushed? repo)))
      (testing "an edit is published with the date that the post had, and an unfinished edit of
                another post stays where it is"
        (spit (fs/file dir "posts" "hello-bareblog" "post.md") "# Hello again\n\nNew text.\n")
        (spit (fs/file dir "posts" "why-web-components" "post.md") "# Not finished\n")
        (is (= :published (:outcome (publish! repo {:folder "hello-bareblog"} true))))
        (is (= {:title "Hello again" :date now}
               (select-keys (listed dir "hello-bareblog") [:title :date])))
        (is (= "Why web components" (:title (listed dir "why-web-components"))))
        (is (= "Why web components"
               (get-in (edn/read-string (slurp (fs/file dir "docs" "why-web-components" "page.edn")))
                       [:text :title])))
        (is (not (contains? (:paths (committed dir)) "posts/why-web-components/post.md")))
        (is (= "M posts/why-web-components/post.md" (sh dir "status" "--porcelain"))))
      (testing "the whole blog asks about that edit, and with a no it stays unpublished"
        (let [{:keys [outcome asked]} (publish! repo {:all? true} false)]
          (is (= :unchanged outcome))
          (is (= 1 (count asked)))
          (is (re-find #"not published: why-web-components\." (first asked)))
          (is (= "Why web components" (:title (listed dir "why-web-components"))))
          (is (not (clean? dir)))))
      (testing "with a yes it is published"
        (publish! repo {:all? true} true)
        (is (= "Not finished" (:title (listed dir "why-web-components"))))
        (is (clean? dir))))))

(deftest removal-test
  (with-repo
    (fn [{:keys [dir] :as repo}]
      (publish! repo {:all? true} true)
      (fs/move (fs/path dir "posts" "hello-bareblog") (fs/path dir "drafts" "hello-bareblog"))
      (testing "a removal asks, and with a no nothing is done"
        (let [before                  (head dir)
              {:keys [outcome asked]} (publish! repo {:folder "hello-bareblog"} false)]
          (is (= :declined outcome))
          (is (= ["Remove from the published blog: hello-bareblog?"] asked))
          (is (some #{"hello-bareblog"} (folders dir)))
          (is (= before (head dir)))))
      (testing "with a yes the post is gone from the list and from the pages, and its folder from git"
        (is (= :published (:outcome (publish! repo {:folder "hello-bareblog"} true))))
        (is (not-any? #{"hello-bareblog"} (folders dir)))
        (is (not (fs/exists? (fs/path dir "docs" "hello-bareblog"))))
        (is (= "Remove hello-bareblog" (:message (committed dir))))
        (is (contains? (:paths (committed dir)) "posts/hello-bareblog/post.md"))
        (is (fs/exists? (fs/path dir "drafts" "hello-bareblog" "post.md")))
        (is (pushed? repo))))))

(deftest page-test
  (with-repo
    (fn [{:keys [dir] :as repo}]
      (testing "the whole blog holds its pages"
        (is (= :published (:outcome (publish! repo {:all? true} true))))
        (is (= ["about"] (map :folder (:pages (site dir)))))
        (is (= "/my-blog/about/" (slurp (fs/file dir "docs" "about" "index.html"))))
        (is (= {:kind :standalone :folder "about"}
               (:page (edn/read-string (slurp (fs/file dir "docs" "about" "page.edn"))))))
        (is (fs/regular-file? (fs/path dir "docs" "about" "boat.svg")))
        (is (not (fs/exists? (fs/path dir "docs" "about" "page.md"))))
        (is (contains? (:paths (committed dir)) "docs/about/index.html"))
        (is (clean? dir)))
      (testing "a new page is published by its name, from where it is, and gets no date"
        (fs/create-dirs (fs/path dir "pages" "contact"))
        (spit (fs/file dir "pages" "contact" "page.md") "# Contact\n")
        (let [{:keys [outcome message]} (publish! repo {:folder "contact"} true)]
          (is (= [:published "Publish contact"] [outcome message]))
          (is (= ["about" "contact"] (map :folder (:pages (site dir)))))
          (is (= [now now] (map :date (:pages (site dir)))))
          (is (fs/directory? (fs/path dir "pages" "contact")))
          (is (contains? (:paths (committed dir)) "pages/contact/page.md"))
          (is (pushed? repo))))
      (testing "an edit of a page is published, and an unfinished edit of another page stays where it is"
        (spit (fs/file dir "pages" "contact" "page.md") "# Get in touch\n")
        (spit (fs/file dir "pages" "about" "page.md") "# Not finished\n")
        (is (= :published (:outcome (publish! repo {:folder "contact"} true))))
        (is (= #{"Get in touch" "About"} (set (map :title (:pages (site dir))))))
        (is (= "M pages/about/page.md" (sh dir "status" "--porcelain"))))
      (testing "a page whose folder is gone is removed, after a yes"
        (fs/delete-tree (fs/path dir "pages" "contact"))
        (let [{:keys [outcome asked]} (publish! repo {:folder "contact"} true)]
          (is (= :published outcome))
          (is (= ["Remove from the published blog: contact?"] asked))
          (is (= ["about"] (map :folder (:pages (site dir)))))
          (is (not (fs/exists? (fs/path dir "docs" "contact"))))
          (is (= "Remove contact" (:message (committed dir)))))))))

(deftest page-becomes-post-test
  (with-repo
    (fn [{:keys [dir] :as repo}]
      (publish! repo {:all? true} true)
      (fs/create-dirs (fs/path dir "posts"))
      (fs/move (fs/path dir "pages" "about") (fs/path dir "posts" "about"))
      (fs/move (fs/path dir "posts" "about" "page.md") (fs/path dir "posts" "about" "post.md"))
      (testing "a published page whose folder is moved to the posts is published as a post, and asks nothing"
        (let [{:keys [outcome asked]} (publish! repo {:folder "about"} true)]
          (is (= :published outcome))
          (is (empty? asked))
          (is (empty? (:pages (site dir))))
          (is (= now (:date (listed dir "about"))))
          (is (= :post (get-in (edn/read-string (slurp (fs/file dir "docs" "about" "page.edn"))) [:page :kind])))))
      (testing "both the place it left and the place it is in are committed"
        (is (contains? (:paths (committed dir)) "posts/about/post.md"))
        (is (= "" (sh dir "ls-files" "pages/about")))
        (is (clean? dir))))))

(deftest held-back-page-test
  (with-repo
    (fn [{:keys [dir] :as repo}]
      (publish! repo {:all? true} true)
      (spit (fs/file dir "pages" "about" "page.md") "# Not finished\n")
      (testing "the whole blog asks about an edited page too, and with a no it stays as it was published"
        (let [{:keys [outcome asked]} (publish! repo {:all? true} false)]
          (is (= :unchanged outcome))
          (is (re-find #"not published: about\." (first asked)))
          (is (= ["About"] (map :title (:pages (site dir)))))
          (is (= "M pages/about/page.md" (sh dir "status" "--porcelain")))))
      (testing "with a yes it is published"
        (is (= :published (:outcome (publish! repo {:all? true} true))))
        (is (= ["Not finished"] (map :title (:pages (site dir)))))
        (is (clean? dir))))))

(deftest two-questions-test
  (with-repo
    (fn [{:keys [dir] :as repo}]
      (publish! repo {:all? true} true)
      (spit (fs/file dir "posts" "why-web-components" "post.md") "# Not finished\n")
      (fs/delete-tree (fs/path dir "posts" "hello-bareblog"))
      (testing "the whole blog asks of the edits and of what is removed, and each has its own answer"
        (let [{:keys [outcome asked]} (publish! repo {:all? true} (partial re-find #"^Remove"))]
          (is (= :published outcome))
          (is (= 2 (count asked)))
          (is (not-any? #{"hello-bareblog"} (folders dir)))
          (is (= "Why web components" (:title (listed dir "why-web-components"))))
          (is (= "M posts/why-web-components/post.md" (sh dir "status" "--porcelain"))))))))

(deftest refusals-test
  (with-repo
    (fn [{:keys [dir app] :as repo}]
      (publish! repo {:all? true} true)
      (let [before (head dir)]
        (testing "a name that is nowhere is refused"
          (let [{:keys [outcome why]} (publish! repo {:folder "zebra"} true)]
            (is (= :refused outcome))
            (is (re-find #"nothing named zebra" why))))
        (testing "what is no name of a post folder is refused, though it leads to a directory"
          (let [{:keys [outcome why]} (publish! repo {:folder "../docs"} true)]
            (is (= :refused outcome))
            (is (re-find #"no name of a folder" why))))
        (testing "a post with something wrong is not published, and a draft is moved back"
          (fs/create-dirs (fs/path dir "drafts" "bad-tag"))
          (spit (fs/file dir "drafts" "bad-tag" "post.md") "Text.\n\n#a/b\n")
          (let [{:keys [outcome why said]} (publish! repo {:folder "bad-tag"} true)]
            (is (= :failed outcome))
            (is (re-find #"The tag \"a/b\" gets no page" why))
            (is (str/ends-with? (last said) "Nothing was published."))
            (is (fs/exists? (fs/path dir "drafts" "bad-tag" "post.md")))
            (is (not (fs/exists? (fs/path dir "posts" "bad-tag"))))
            (is (not-any? #{"bad-tag"} (folders dir)))))
        (testing "a draft that cannot be read at all is moved back too"
          (fs/create-dirs (fs/path dir "drafts" "no-text"))
          (let [{:keys [outcome]} (publish! repo {:folder "no-text"} true)]
            (is (= :failed outcome))
            (is (fs/directory? (fs/path dir "drafts" "no-text")))
            (is (not (fs/exists? (fs/path dir "posts" "no-text"))))))
        (testing "a directory that is no repository of its own is refused, and nothing is written"
          (let [inside (str (fs/create-dirs (fs/path dir "inside")))]
            (spit (fs/file inside "blog.edn") "{}")
            (is (= :refused (:outcome (publish! {:dir inside :app app} {:all? true} true))))
            (is (not (fs/exists? (fs/path inside "docs"))))))
        (testing "none of these committed anything"
          (is (= before (head dir))))
        (testing "something wrong with another folder is said, and does not stop a publish"
          (fs/create-dirs (fs/path dir "posts" "Bad Name"))
          (let [{:keys [outcome said]} (publish! repo {:folder "a-new-idea"} true)]
            (is (= :published outcome))
            (is (some #(re-find #"posts/Bad Name is left out" %) said))))))))

(deftest push-fails-test
  (with-repo
    (fn [{:keys [dir remote] :as repo}]
      (fs/delete-tree remote)
      (let [{:keys [outcome message said]} (publish! repo {:all? true} true)]
        (testing "a push that fails leaves the commit, and says how to push it"
          (is (= [:not-pushed "Publish the whole blog"] [outcome message]))
          (is (not (publish/done? outcome)))
          (is (= "Publish the whole blog" (:message (committed dir))))
          (is (re-find #"could not push" (last said)))
          (is (re-find #"git push" (last said))))))))

(deftest git-test
  (with-repo
    (fn [{:keys [dir remote]}]
      (testing "the top of a repository is one, and a directory inside it is not"
        (is (git/top? dir))
        (is (not (git/top? (fs/path dir "posts"))))
        (is (not (git/top? (fs/parent dir)))))
      (testing "a commit holds what changed under its paths, and nothing else"
        (spit (fs/file dir "posts" "hello-bareblog" "post.md") "# Changed\n")
        (spit (fs/file dir "blog.edn") "{:title \"Other\"}")
        (is (= {:committed? true} (git/commit! dir ["posts/hello-bareblog" "not-there"] "One post")))
        (is (= {:message "One post" :paths #{"posts/hello-bareblog/post.md"}} (committed dir)))
        (is (= "M blog.edn" (sh dir "status" "--porcelain"))))
      (testing "paths with nothing changed under them commit nothing"
        (is (= {:committed? false} (git/commit! dir ["posts"] "Nothing"))))
      (testing "a push sends the branch, and says why when it cannot"
        (is (= {} (git/push! dir)))
        (is (= (head dir) (sh remote "rev-parse" "main")))
        (fs/delete-tree remote)
        (is (re-find #"^fatal:" (:problem (git/push! dir))))))))
