(ns bareblog.command.comments-test
  (:require [babashka.fs :as fs]
            [bareblog.blog.wire :as wire]
            [bareblog.command.comments :as comments]
            [bareblog.example :as example]
            [bareblog.read.source :as source]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(deftest script-text-test
  (let [text (comments/script-text (slurp "resources/comments/inbox.gs"))]
    (testing "the script holds the version and the limits of this BareBlog, and no placeholder"
      (is (str/includes? text (str "var VERSION = " comments/script-version ";")))
      (is (str/includes? text (str "var TEXT_MAX = " (:text-max wire/comment-limits) ";")))
      (is (str/includes? text (str "var OPEN_MIN_MS = " (:open-min-ms wire/comment-limits) ";")))
      (is (not (str/includes? text "{{"))))))

(deftest address-test
  (testing "the address of a web app is taken, with the spaces around it"
    (is (= {:address "https://script.google.com/macros/s/AKfy_c-1/exec"}
           (comments/address "  https://script.google.com/macros/s/AKfy_c-1/exec \n"))))
  (testing "nothing stops"
    (is (= {:stop true} (comments/address "")))
    (is (= {:stop true} (comments/address nil))))
  (testing "the test address, the address of the sheet and anything else are each told apart"
    (is (re-find #"/dev" (:wrong (comments/address "https://script.google.com/macros/s/AKfy/dev"))))
    (is (re-find #"of the sheet" (:wrong (comments/address "https://docs.google.com/spreadsheets/d/1x/edit"))))
    (is (re-find #"no web app" (:wrong (comments/address "http://script.google.com/macros/s/AKfy/exec"))))))

(deftest answer-test
  (testing "an answer in JSON with ok is the script"
    (is (= {:script {:ok false :error "wrong secret" :version 1}}
           (comments/answer {:status 200 :body "{\"ok\":false,\"error\":\"wrong secret\",\"version\":1}"}))))
  (testing "a refusal, nothing, a page and no answer each say what is wrong"
    (is (re-find #"403" (:wrong (comments/answer {:status 403 :body "<html>"}))))
    (is (re-find #"404" (:wrong (comments/answer {:status 404 :body "<html>"}))))
    (is (re-find #"a page" (:wrong (comments/answer {:status 200 :body "<html>"}))))
    (is (re-find #"not be reached" (:wrong (comments/answer {:unreachable "timed out"}))))))

(deftest older-test
  (testing "a script with a lower version, or none, is older"
    (is (comments/older? {:version (dec comments/script-version)}))
    (is (comments/older? {}))
    (is (not (comments/older? {:version comments/script-version})))))

(deftest check-test
  (testing "the script takes its state, or says why not"
    (is (nil? (comments/state-problem true {:ok true :open true})))
    (is (re-find #"did not take" (comments/state-problem true {:ok true :open false})))
    (is (re-find #"wrong secret" (comments/state-problem false {:ok false :error "wrong secret"}))))
  (testing "with comments on, a test comment is taken with an id, and with comments off it is refused"
    (is (nil? (comments/sent-problem true {:ok true :id "a"})))
    (is (re-find #"no id" (comments/sent-problem true {:ok true})))
    (is (re-find #"refused" (comments/sent-problem true {:ok false :error "comments are closed"})))
    (is (nil? (comments/sent-problem false {:ok false :error "comments are closed"})))
    (is (re-find #"while comments are off" (comments/sent-problem false {:ok true :id "a"}))))
  (testing "the test comment is in the rows, and is deleted"
    (is (nil? (comments/read-problem "a" {:rows [{:id "b"} {:id "a"}]})))
    (is (re-find #"did not reach" (comments/read-problem "a" {:rows [{:id "b"}]})))
    (is (nil? (comments/deleted-problem {:deleted true})))
    (is (re-find #"by hand" (comments/deleted-problem {:deleted false}))))
  (testing "the test comment is open long enough, names no post, and is no trap"
    (is (< (:open-min-ms wire/comment-limits) (:open comments/test-comment)))
    (is (= "" (:post comments/test-comment) (:trap comments/test-comment)))))

(deftest check-problem-test
  (let [ok      {:script {:ok true :open true :version 1}}
        sent    {:script {:ok true :id "t"}}
        rows    {:script {:ok true :rows [{:id "t"}]}}
        deleted {:script {:ok true :deleted true}}
        on      {:state ok :sent sent :rows rows :deleted deleted}]
    (testing "a check with comments on finds nothing when every step went well"
      (is (nil? (comments/check-problem true on))))
    (testing "a check gives the first thing that went wrong, in the order of its steps"
      (is (re-find #"could not be reached" (comments/check-problem true (assoc on :state {:wrong "The address could not be reached: x"}))))
      (is (re-find #"did not reach" (comments/check-problem true (assoc on :rows {:script {:ok true :rows []}}))))
      (is (re-find #"by hand" (comments/check-problem true (assoc on :deleted {:script {:ok true :deleted false}})))))
    (testing "a check with comments off wants the test comment refused, and reads no rows"
      (is (nil? (comments/check-problem false {:state {:script {:ok true :open false}}
                                               :sent  {:script {:ok false :error "comments are closed"}}})))
      (is (re-find #"while comments are off"
                   (comments/check-problem false {:state {:script {:ok true :open false}} :sent sent}))))))

(defn- switched
  "What `switch!` says, gives and leaves in the settings, in a copy of the example blog whose
   setting of comments is `setting`, when the author answers `yes?` and comments go `on?`."
  [setting yes? on?]
  (example/with-copy (cond-> {} setting (assoc :comments setting))
    (fn [dir]
      (let [said      (atom [])
            published (atom 0)
            how       {:say          #(swap! said conj %)
                       :ask          (constantly yes?)
                       :publish-all! #(do (swap! published inc) 0)}
            gave      (comments/switch! how {:dir dir} on?)]
        {:gave      gave
         :said      @said
         :published @published
         :setting   (:comments (source/read-settings dir))}))))

(deftest switch-test
  (testing "comments go on only once they are connected"
    (let [{:keys [gave said published setting]} (switched nil true true)]
      (is (= 1 gave))
      (is (re-find #"not connected" (first said)))
      (is (zero? published))
      (is (nil? setting))))
  (testing "comments that are off already are left as they are"
    (is (= 0 (:gave (switched {:on? false} true false)))))
  (testing "a no changes nothing"
    (let [{:keys [gave published setting]} (switched {:on? true} false false)]
      (is (= 1 gave))
      (is (zero? published))
      (is (= {:on? true} setting))))
  (testing "comments go off in the settings and on every page"
    (let [{:keys [gave published setting]} (switched {:on? true} true false)]
      (is (= 0 gave))
      (is (= 1 published))
      (is (= {:on? false} setting)))))

(deftest connect-test
  (testing "a wrong address that the author typed is told, and asked for again, and nothing stops
            with nothing written"
    (example/with-copy {}
      (fn [dir]
        (let [said    (atom [])
              opened  (atom [])
              lines   (atom ["https://docs.google.com/spreadsheets/d/1x/edit"
                             "https://script.google.com/macros/s/AKfy/dev"
                             ""])
              how     {:say     #(swap! said conj %)
                       :browse! #(swap! opened conj %)
                       :prompt  (fn [_] (let [line (first @lines)] (swap! lines rest) line))}
              gave    (comments/connect! how {:dir dir} nil)]
          (is (= 1 gave))
          (is (= [comments/template-link] @opened))
          (is (empty? @lines))
          (is (some #(re-find #"address of the sheet" %) @said))
          (is (some #(re-find #"/dev" %) @said))
          (is (= "Nothing was changed." (last @said)))
          (is (nil? (:comments (source/read-settings dir))))))))
  (testing "a wrong address on the command line is told, and not asked for again"
    (example/with-copy {}
      (fn [dir]
        (let [said (atom [])
              how  {:say    #(swap! said conj %)
                    :prompt (fn [_] (throw (ex-info "asked again" {})))}]
          (is (= 1 (comments/connect! how {:dir dir} "https://docs.google.com/x")))
          (is (re-find #"address of the sheet" (first @said)))
          (is (= "Nothing was changed." (last @said))))))))

(defn- replied
  "What `reply!` says, gives and publishes, in a copy of the example blog with comments `on?`
   whose published posts are its post of Lisbon, for the short id `short` and the reply `text`,
   with what the editor gives as `edited` and the answer `yes?`."
  [{:keys [on? short text edited yes?] :or {on? true yes? true}}]
  (example/with-copy {:comments {:script "https://s" :on? on?}}
    (fn [dir]
      (fs/create-dirs (fs/path dir "docs"))
      (spit (fs/file dir "docs" "site.edn") (pr-str {:posts [{:folder "a-week-in-lisbon"}]}))
      (let [said      (atom [])
            published (atom [])
            how       {:say               #(swap! said conj %)
                       :ask               (constantly yes?)
                       :edit!             (fn [text] (str edited text))
                       :publish-comments! #(do (swap! published conj %) 0)}
            gave      (comments/reply! how {:dir dir} short text)]
        {:gave gave :said @said :published @published}))))

(deftest reply-test
  (testing "a reply is published with the comments of its post, and answers the comment it is to"
    (let [{:keys [gave said published]} (replied {:short "1f0c9a52" :text " Thank you. "})
          [{:keys [folder message] :as request}] published
          reply (last (:comments request))]
      (is (= 0 gave))
      (is (re-find #"^Ann Lee, 2026-10-20, on a-week-in-lisbon:\nTram 28" (first said)))
      (is (= ["a-week-in-lisbon" "Reply to a comment on a-week-in-lisbon"] [folder message]))
      (is (= 7 (count (:comments request))))
      (is (= {:reply-to "1f0c9a52-6d3e-4b1a-9a7e-2c4d5e6f7a81" :author? true :text "Thank you."}
             (select-keys reply [:reply-to :author? :text])))
      (is (re-matches #"\d{4}-\d\d-\d\dT\d\d:\d\d:\d\d(\.\d{3})?Z" (:moment reply)))))
  (testing "with no text, the reply is what the author writes in the editor"
    (is (= "From the editor."
           (-> (replied {:short "1f0c9a52" :edited "From the editor.\n"}) :published first :comments last :text))))
  (testing "an empty reply and a no publish nothing"
    (is (empty? (:published (replied {:short "1f0c9a52" :edited "  \n"}))))
    (is (empty? (:published (replied {:short "1f0c9a52" :text "Hi." :yes? false})))))
  (testing "a short id of no comment, or of more than one, is refused"
    (is (re-find #"No published comment" (last (:said (replied {:short "ffff" :text "Hi."})))))
    (is (re-find #"More than one comment" (last (:said (replied {:short "" :text "Hi."}))))))
  (testing "while comments are off there is no reply"
    (let [{:keys [gave said published]} (replied {:on? false :short "1f0c9a52" :text "Hi."})]
      (is (= 1 gave))
      (is (re-find #"Comments are off" (first said)))
      (is (empty? published)))))

(deftest reply-of-test
  (let [opened (comments/editor-text "Ann Lee, 2026-10-20, on a-week-in-lisbon:\nTram 28.")]
    (testing "the editor opens on the comment, below the cut line, with room above it"
      (is (str/starts-with? opened (str "\n\n" comments/cut-line "\nAnn Lee"))))
    (testing "the reply is what the author wrote above the cut line"
      (is (= "Thank you." (comments/reply-of (str "Thank you.\n" opened))))
      (is (= "# A heading\n\nAnd a text." (comments/reply-of (str "# A heading\n\nAnd a text.\n" opened)))))
    (testing "nothing written, or nothing saved, is an empty reply"
      (is (= "" (comments/reply-of opened)))
      (is (= "" (comments/reply-of nil))))
    (testing "a text with no cut line is the reply whole"
      (is (= "Hi." (comments/reply-of "Hi.\n"))))))

(defn- removed
  "What `remove!` says, gives and publishes, in a copy of the example blog with comments `on?`
   whose published posts are its post of Lisbon, for the short id `short` and the answer `yes?`."
  [{:keys [on? short yes?] :or {on? true yes? true}}]
  (example/with-copy {:comments {:script "https://s" :on? on?}}
    (fn [dir]
      (fs/create-dirs (fs/path dir "docs"))
      (spit (fs/file dir "docs" "site.edn") (pr-str {:posts [{:folder "a-week-in-lisbon"}]}))
      (let [said      (atom [])
            published (atom [])
            how       {:say               #(swap! said conj %)
                       :ask               (fn [question] (swap! said conj question) yes?)
                       :publish-comments! #(do (swap! published conj %) 0)}
            gave      (comments/remove! how {:dir dir} short)]
        {:gave gave :said @said :published @published}))))

(deftest remove-test
  (testing "a comment goes with every reply under it, and its post is published without them"
    (let [{:keys [gave said published]} (removed {:short "2a7d3b64"})
          [{:keys [folder message] :as request}] published]
      (is (= 0 gave))
      (is (re-find #"^Bo Chen, 2026-10-21" (first said)))
      (is (= "Remove it and its 3 replies from a-week-in-lisbon?" (second said)))
      (is (= ["a-week-in-lisbon" "Remove a comment on a-week-in-lisbon"] [folder message]))
      (is (= ["1f0c9a52-6d3e-4b1a-9a7e-2c4d5e6f7a81" "6eb17fa8-c25d-406f-8fd1-7b9cadbecfd6"]
             (map :id (:comments request))))))
  (testing "a comment with one reply is asked about with it"
    (is (= "Remove it and its reply from a-week-in-lisbon?" (second (:said (removed {:short "4c9f5d86"}))))))
  (testing "a comment with no replies goes alone, and is asked about alone"
    (let [{:keys [said published]} (removed {:short "6eb17fa8"})]
      (is (= "Remove it from a-week-in-lisbon?" (second said)))
      (is (= 5 (count (:comments (first published)))))))
  (testing "a no publishes nothing"
    (is (empty? (:published (removed {:short "2a7d3b64" :yes? false})))))
  (testing "a comment is removed also while comments are off"
    (is (= 1 (count (:published (removed {:on? false :short "6eb17fa8"}))))))
  (testing "a short id of no comment is refused"
    (is (re-find #"No published comment" (last (:said (removed {:short "ffff"})))))))
