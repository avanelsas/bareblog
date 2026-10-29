(ns bareblog.command.cli-test
  (:require [babashka.fs :as fs]
            [babashka.process :as process]
            [bareblog.command.cli :as cli]
            [bareblog.example :as example]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(deftest asked-test
  (testing "comments are connected, with or without an address, checked, and switched"
    (is (= {:command "comments" :connect true :address nil :yes? false} (cli/asked ["comments" "connect"])))
    (is (= {:command "comments" :connect true :address "https://a" :yes? false}
           (cli/asked ["comments" "connect" "https://a"])))
    (is (= {:command "comments" :check true} (cli/asked ["comments" "check"])))
    (is (= {:command "comments" :switch true :on? true :yes? true} (cli/asked ["comments" "--on" "--yes"])))
    (is (= {:command "comments" :switch true :on? false :yes? false} (cli/asked ["comments" "--off"])))
    (is (every? (comp some? :refuse cli/asked)
                [["comments"] ["comments" "reply"] ["comments" "reply" "a" "b" "c"] ["comments" "check" "x"]
                 ["comments" "--on" "x"] ["comments" "remove"] ["comments" "remove" "a" "b"]])))
  (testing "a removal is asked for with the short id of its comment"
    (is (= {:command "comments" :remove true :short-id "1f0c9a52" :yes? true}
           (cli/asked ["comments" "remove" "1f0c9a52" "--yes"]))))
  (testing "a reply is asked for with the short id of its comment, and its text or none"
    (is (= {:command "comments" :reply true :short-id "1f0c9a52" :text "Thanks." :yes? false}
           (cli/asked ["comments" "reply" "1f0c9a52" "Thanks."])))
    (is (= {:command "comments" :reply true :short-id "1f0c9a52" :text nil :yes? true}
           (cli/asked ["comments" "reply" "1f0c9a52" "--yes"]))))
  (testing "a new blog is asked for with its title"
    (is (= {:command "init" :title "My blog"} (cli/asked ["init" "My blog"])))
    (is (re-find #"in quotes" (:refuse (cli/asked ["init"]))))
    (is (some? (:refuse (cli/asked ["init" "My" "blog"])))))
  (testing "a new post is asked for with its title"
    (is (= {:command "new" :title "A new blog post" :page? false} (cli/asked ["new" "A new blog post"]))))
  (testing "a new page is asked for with its title and the flag for one, in either order"
    (is (= {:command "new" :title "About" :page? true} (cli/asked ["new" "--page" "About"])))
    (is (= {:command "new" :title "About" :page? true} (cli/asked ["new" "About" "--page"])))
    (is (some? (:refuse (cli/asked ["new" "--page"])))))
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
  (testing "the tags are listed, or one is renamed with its new name"
    (is (= {:command "tags"} (cli/asked ["tags"])))
    (is (= {:command "tags" :rename ["baredom" "BareDOM"] :yes? false} (cli/asked ["tags" "rename" "baredom" "BareDOM"])))
    (is (= {:command "tags" :rename ["a" "b"] :yes? true} (cli/asked ["tags" "rename" "--yes" "a" "b"]))))
  (testing "a rename without both names, or with more, is refused"
    (is (every? (comp some? :refuse cli/asked)
                [["tags" "rename"] ["tags" "rename" "a"] ["tags" "rename" "a" "b" "c"] ["tags" "list"]
                 ["tags" "--yes"]])))
  (testing "the series are listed, or one is named with its posts, or with none to remove it"
    (is (= {:command "series"} (cli/asked ["series"])))
    (is (= {:command "series" :series-name "Trip" :named ["The start" "end"] :yes? false}
           (cli/asked ["series" "--name" "Trip" "The start" "end"])))
    (is (= {:command "series" :series-name "Trip" :named [] :yes? true} (cli/asked ["series" "--name" "Trip" "--yes"]))))
  (testing "the font is said, fetched by its name, or taken away"
    (is (= {:command "font"} (cli/asked ["font"])))
    (is (= {:command "font" :family "Source Serif 4" :yes? false} (cli/asked ["font" "Source Serif 4"])))
    (is (= {:command "font" :family "Literata" :yes? true} (cli/asked ["font" "--yes" "Literata"])))
    (is (= {:command "font" :none? true :yes? true} (cli/asked ["font" "--none" "--yes"]))))
  (testing "a font that is no name, or more than one, is refused"
    (doseq [args [["font" "Bad\"name"] ["font" "A" "B"] ["font" "--none" "A"] ["font" "--yes"]]]
      (is (re-find #"Give the name of a font" (:refuse (cli/asked args))) (pr-str args))))
  (testing "a series without --name and its name is refused"
    (is (every? (comp some? :refuse cli/asked)
                [["series" "Trip"] ["series" "--name"] ["series" "--yes"] ["series" "--nam" "Trip"]])))
  (testing "the version is asked for, and an update with or without a yes to its question"
    (is (= {:command "version"} (cli/asked ["version"])))
    (is (= {:command "update" :yes? false} (cli/asked ["update"])))
    (is (= {:command "update" :yes? true} (cli/asked ["update" "--yes"]))))
  (testing "an update with anything else is refused"
    (is (some? (:refuse (cli/asked ["update" "main"])))))
  (testing "no command, and help, ask for how the command is used"
    (is (= {:command "help"} (cli/asked [])))
    (is (= {:command "help"} (cli/asked ["help"]))))
  (testing "a command that there is not is refused, with its name"
    (is (= {:refuse "There is no command publish-everything."} (cli/asked ["publish-everything"])))))

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
