(ns bareblog.cli-test
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [bareblog.cli :as cli]
            [bareblog.example :as example]
            [bareblog.home :as home]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(deftest asked-test
  (testing "a new post is asked for with its title"
    (is (= {:command "new" :title "A new blog post"} (cli/asked ["new" "A new blog post"]))))
  (testing "a title in more words than one, or none, is refused with how to give it"
    (is (some? (:refuse (cli/asked ["new" "A" "new" "post"]))))
    (is (some? (:refuse (cli/asked ["new" "  "]))))
    (is (re-find #"in quotes" (:refuse (cli/asked ["new"])))))
  (testing "a preview is on the usual port and opens the browser, until it is told otherwise"
    (is (= {:command "preview" :port 8080 :open? true} (cli/asked ["preview"])))
    (is (= {:command "preview" :port 9000 :open? true} (cli/asked ["preview" "9000"])))
    (is (= {:command "preview" :port 9000 :open? false} (cli/asked ["preview" "--no-open" "9000"])))
    (is (= {:command "preview" :port 8080 :open? false} (cli/asked ["preview" "--no-open"]))))
  (testing "what is no port, and more than a port, are refused"
    (is (every? (comp some? :refuse cli/asked)
                [["preview" "nine"] ["preview" "0"] ["preview" "65536"] ["preview" "123456"]
                 ["preview" "9000" "9001"]]))
    (is (= 65535 (:port (cli/asked ["preview" "65535"])))))
  (testing "a publish is of one post, by its folder, or of the whole blog"
    (is (= {:command "publish" :folder "a-week-in-lisbon" :yes? false} (cli/asked ["publish" "a-week-in-lisbon"])))
    (is (= {:command "publish" :all? true :yes? false} (cli/asked ["publish" "--all"])))
    (is (= {:command "publish" :all? true :yes? true} (cli/asked ["publish" "--yes" "--all"])))
    (is (= {:command "publish" :folder "a" :yes? true} (cli/asked ["publish" "a" "--yes"]))))
  (testing "a publish of nothing, of two things, or with a flag that there is not, is refused"
    (is (every? (comp some? :refuse cli/asked)
                [["publish"] ["publish" "a" "b"] ["publish" "--all" "a"] ["publish" "--everything"]
                 ["publish" "--yes"]])))
  (testing "no command, and help, ask for how the command is used"
    (is (= {:command "help"} (cli/asked [])))
    (is (= {:command "help"} (cli/asked ["help"]))))
  (testing "a command that there is not is refused, with its name"
    (is (= {:refuse "There is no command publish-everything."} (cli/asked ["publish-everything"])))))

(deftest draft-test
  (testing "a draft has a folder named for its title, and a post that starts with the title"
    (is (= {:folder "a-new-blog-post" :text "# A new blog post\n\n"} (cli/draft "A new blog post")))
    (is (= {:folder "cafe-lisboa-2026" :text "# Café Lisboa, 2026!\n\n"} (cli/draft " Café Lisboa, 2026! "))))
  (testing "a title that gives no name for a folder is a problem"
    (is (some? (:problem (cli/draft "!!!"))))
    (is (some? (:problem (cli/draft "Tags"))))
    (is (nil? (:folder (cli/draft "Archive"))))))

(deftest new-draft-test
  (example/with-copy {}
    (fn [dir]
      (let [made (cli/new-draft! dir "A third thought")]
        (testing "the draft is a folder in the drafts of the blog, with its post"
          (is (= (str (fs/path dir "drafts" "a-third-thought" "post.md")) (:file made)))
          (is (= "# A third thought\n\n" (slurp (:file made)))))
        (testing "a title of a post that is there already, as a draft or published, makes nothing"
          (is (re-find #"already" (:problem (cli/new-draft! dir "A third thought"))))
          (is (re-find #"already" (:problem (cli/new-draft! dir "A week in Lisbon"))))
          (is (= "# A third thought\n\n" (slurp (:file made)))))
        (testing "a title that gives no folder makes nothing"
          (is (some? (:problem (cli/new-draft! dir "???")))))))))

(def ^:private command
  "The command, as a blog author runs it."
  (str (fs/absolutize (fs/path "bin" "bareblog"))))

(defn- run-in
  "What the command says and ends with, when it is run in the directory `dir` with `args`."
  [dir & args]
  (select-keys (apply process/shell {:dir dir :out :string :err :string :continue true} command args)
               [:exit :out]))

;; The command is run as an author runs it, from a directory that is not where BareBlog is.
(deftest command-test
  (example/with-copy {}
    (fn [dir]
      (testing "it makes a draft in the blog that it is run in"
        (is (zero? (:exit (run-in dir "new" "From another place"))))
        (is (= "# From another place\n\n" (slurp (fs/file dir "drafts" "from-another-place" "post.md")))))))
  (testing "outside a blog it says that the directory is none, and how it is used"
    (let [dir (str (fs/create-temp-dir))]
      (try
        (let [{:keys [exit out]} (run-in dir "new" "A post")]
          (is (= 1 exit))
          (is (str/includes? out "No blog.edn in"))
          (is (str/includes? out cli/usage)))
        (finally
          (fs/delete-tree dir))))))

(defn- run
  "What the command says and ends with, for the arguments `args`, without a process of its own."
  [& args]
  (let [exit (atom nil)
        out  (with-out-str (reset! exit (cli/main "." args)))]
    {:exit @exit :out out}))

(deftest main-test
  (testing "what is refused is said with how the command is used, and ends with one"
    (let [{:keys [exit out]} (run "frobnicate")]
      (is (= 1 exit))
      (is (str/includes? out "There is no command frobnicate."))
      (is (str/includes? out cli/usage))))
  (testing "help says how the command is used, and ends with nothing wrong"
    (is (= {:exit 0 :out (str cli/usage "\n")} (run "help")))
    (is (= {:exit 0 :out (str cli/usage "\n")} (run)))))

(deftest home-test
  (testing "the files of the app are in BareBlog, wherever that is"
    (is (= "/somewhere/bareblog/resources/public" (str (home/app-dir "/somewhere/bareblog"))))
    (is (fs/regular-file? (fs/path (home/app-dir ".") "index.html")))))
