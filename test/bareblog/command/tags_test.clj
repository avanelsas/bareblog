(ns bareblog.command.tags-test
  (:require [babashka.fs :as fs]
            [bareblog.command.tags :as tags]
            [bareblog.example :as example]
            [bareblog.read.source :as source]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(defn- run
  "What `command` says and ends with for the blog in `dir`, with `answer` to its question."
  [command dir answer & args]
  (let [said (atom [])
        exit (apply command {:say (partial swap! said conj) :ask (constantly answer)} dir args)]
    {:exit exit :said @said}))

(defn- text-of [dir place]
  (slurp (fs/file dir place)))

(defn- write-tags!
  "Writes the text at `place` in `dir` as a text with the paragraph of tags `line`."
  [dir place line]
  (spit (fs/file dir place) (str "# A text\n\nText.\n\n" line "\n")))

(deftest list-test
  (example/with-copy {}
    (fn [dir]
      (write-tags! dir "drafts/a-new-idea/post.md" "#notes #Travel")
      (let [{:keys [exit said]} (run tags/list! dir nil)]
        (testing "every tag is said with how many texts have it, its spellings beside each other"
          (is (zero? exit))
          (is (= ["Travel (1)" "travel (1)"]
                 (take 2 (drop-while (complement (partial re-find #"(?i)^travel ")) said)))))
        (testing "a tag written in more than one way is said, with the rename that writes it one way"
          (is (re-find #"bareblog tags rename \"travel\" \"Travel\"$" (last said)))))
      (testing "a page, which has no tag pages, is not held to the tags of the posts"
        (write-tags! dir "drafts/a-new-idea/post.md" "#notes")
        (write-tags! dir "pages/about/page.md" "#Travel")
        (is (not-any? (partial re-find #"one tag") (:said (run tags/list! dir nil))))))))

(deftest rename-test
  (example/with-copy {}
    (fn [dir]
      (let [lisbon "posts/a-week-in-lisbon/post.md"
            idea   "drafts/a-new-idea/post.md"
            web    "posts/why-web-components/post.md"
            before (text-of dir lisbon)]
        (write-tags! dir idea "#notes #Travel")
        (write-tags! dir web "#web #travel")
        (testing "a rename that the author says no to changes nothing, after it shows what would change"
          (let [{:keys [exit said]} (run tags/rename! dir false "Travel" "travel")]
            (is (= 1 exit))
            (is (= ["drafts/a-new-idea: #notes #Travel" "Nothing was changed."]
                   [(first said) (last said)]))
            (is (str/ends-with? (text-of dir idea) "#Travel\n"))))
        (testing "a rename is written in every text with the tag, and nothing else in them changes"
          (let [{:keys [exit]} (run tags/rename! dir true "travel" "journeys")]
            (is (zero? exit))
            (is (= (str/replace before "#travel #portugal" "#journeys #portugal") (text-of dir lisbon)))
            (is (str/ends-with? (text-of dir web) "\n#web #journeys\n"))
            (is (str/ends-with? (text-of dir idea) "#Travel\n"))))
        (testing "a rename as a tag written in another way is refused"
          (let [{:keys [exit said]} (run tags/rename! dir true "notes" "Journeys")]
            (is (= 1 exit))
            (is (re-find #"\"Journeys\" is one tag with \"journeys\"" (first said)))))
        (testing "a rename as another spelling of a tag writes the tag one way"
          (is (zero? (:exit (run tags/rename! dir true "Travel" "journeys"))))
          (is (empty? (:problems (source/read-site (source/preview dir))))))
        (testing "what is no tag is no new name, and a name of no tag is not renamed"
          (is (re-find #"cannot be a tag" (first (:said (run tags/rename! dir true "journeys" "a/b")))))
          (is (re-find #"No post or page has the tag \"zebra\"" (first (:said (run tags/rename! dir true "zebra" "z"))))))
        (testing "a page is renamed in as a post is"
          (write-tags! dir "pages/about/page.md" "#journeys")
          (is (zero? (:exit (run tags/rename! dir true "journeys" "trips"))))
          (is (str/ends-with? (text-of dir "pages/about/page.md") "#trips\n")))
        (testing "a name with an underscore is the tag with a space"
          (is (zero? (:exit (run tags/rename! dir true "every_kind" "all_kinds"))))
          (is (str/includes? (text-of dir "posts/every-kind/post.md") "#all_kinds")))))))

(deftest published-test
  (example/with-copy {}
    (fn [dir]
      (fs/create-dirs (fs/path dir "docs"))
      (spit (fs/file dir "docs" "site.edn") (pr-str {:blog {} :posts [{:folder "a-week-in-lisbon"}]}))
      (testing "a published post that a rename changes is named, with how to publish it again"
        (is (= "Published before, and changed now: a-week-in-lisbon. Publish them together with: bareblog publish --all"
               (last (:said (run tags/rename! dir true "portugal" "Portugal")))))))))

(deftest rename-plan-test
  (let [texts [{:place "posts/a" :text "Text.\n\n#travel #web\n" :tags ["travel" "web"]}
               {:place "posts/b" :text "Text.\n\n#web\n" :tags ["web"]}]]
    (testing "a rename gives each text with the tag, with its new tags and its new text"
      (is (= [{:place "posts/a" :new-tags ["journeys" "web"] :written "Text.\n\n#journeys #web\n"}]
             (map #(select-keys % [:place :new-tags :written])
                  (:changes (tags/rename-plan texts "travel" "journeys"))))))
    (testing "a rename that cannot be done gives why, and no changes"
      (is (= {:refusal "No post or page has the tag \"zebra\"."} (tags/rename-plan texts "zebra" "z")))
      (is (re-find #"cannot be a tag" (:refusal (tags/rename-plan texts "web" "a/b")))))))
