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
          [{:keys [message] :as request}] published
          [[folder carried]] (seq (:comments request))
          reply (last carried)]
      (is (= 0 gave))
      (is (re-find #"^Ann Lee, 2026-10-20, on a-week-in-lisbon:\nTram 28" (first said)))
      (is (= ["a-week-in-lisbon" "Reply to a comment on a-week-in-lisbon"] [folder message]))
      (is (= 7 (count carried)))
      (is (= {:reply-to "1f0c9a52-6d3e-4b1a-9a7e-2c4d5e6f7a81" :author? true :text "Thank you."}
             (select-keys reply [:reply-to :author? :text])))
      (is (re-matches #"\d{4}-\d\d-\d\dT\d\d:\d\d:\d\d(\.\d{3})?Z" (:moment reply)))))
  (testing "with no text, the reply is what the author writes in the editor"
    (is (= "From the editor."
           (-> (replied {:short "1f0c9a52" :edited "From the editor.\n"}) :published first :comments vals first last :text))))
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
          [{:keys [message] :as request}] published
          [[folder carried]] (seq (:comments request))]
      (is (= 0 gave))
      (is (re-find #"^Bo Chen, 2026-10-21" (first said)))
      (is (= "Remove it and its 3 replies from a-week-in-lisbon?" (second said)))
      (is (= ["a-week-in-lisbon" "Remove a comment on a-week-in-lisbon"] [folder message]))
      (is (= ["1f0c9a52-6d3e-4b1a-9a7e-2c4d5e6f7a81" "6eb17fa8-c25d-406f-8fd1-7b9cadbecfd6"]
             (map :id carried)))))
  (testing "a comment with one reply is asked about with it"
    (is (= "Remove it and its reply from a-week-in-lisbon?" (second (:said (removed {:short "4c9f5d86"}))))))
  (testing "a comment with no replies goes alone, and is asked about alone"
    (let [{:keys [said published]} (removed {:short "6eb17fa8"})]
      (is (= "Remove it from a-week-in-lisbon?" (second said)))
      (is (= 5 (count (-> published first :comments vals first))))))
  (testing "a no publishes nothing"
    (is (empty? (:published (removed {:short "2a7d3b64" :yes? false})))))
  (testing "a comment is removed also while comments are off"
    (is (= 1 (count (:published (removed {:on? false :short "6eb17fa8"}))))))
  (testing "a short id of no comment is refused"
    (is (re-find #"No published comment" (last (:said (removed {:short "ffff"})))))))

(def ^:private lisbon
  "The published comments of the example post of Lisbon, by its folder, with a post of none."
  {"a-week-in-lisbon" (:comments (source/read-post-comments example/dir "a-week-in-lisbon"))
   "hello-bareblog"   []})

(defn- row
  "A row of the sheet with the id `id`, for the post `post`, that answers `reply-to`, received at
   `received`."
  [id post reply-to received]
  {:id id :received received :post post :replyTo reply-to :name "Fay" :text (str "Text of " id ".")})

(deftest triage-test
  (let [rows [(row "r2" "a-week-in-lisbon" "" "2026-10-29T10:00:00.000Z")
              (row "r1" "hello-bareblog" "" "2026-10-28T10:00:00.000Z")
              (row "r3" "a-week-in-lisbon" "1f0c9a52-6d3e-4b1a-9a7e-2c4d5e6f7a81" "2026-10-29T11:00:00.000Z")
              (row "6eb17fa8-c25d-406f-8fd1-7b9cadbecfd6" "a-week-in-lisbon" "" "2026-10-24T20:05:02.640Z")
              (row "r4" "" "" "2026-10-29T12:00:00.000Z")
              (row "r5" "gone" "" "2026-10-29T12:00:00.000Z")
              (row "r6" "a-week-in-lisbon" "not-there" "2026-10-29T12:00:00.000Z")]
        {:keys [published dropped walk]} (comments/triage rows lisbon)]
    (testing "a row whose id is published is left over"
      (is (= ["6eb17fa8-c25d-406f-8fd1-7b9cadbecfd6"] published)))
    (testing "a row for a post that is not published, or for a comment that is not, is dropped"
      (is (= {:missing-post ["r4" "r5"] :missing-comment ["r6"]} dropped)))
    (testing "the rest are walked through oldest first, as comments of their post"
      (is (= ["r1" "r2" "r3"] (map (comp :id :comment) walk)))
      (is (= {:folder  "a-week-in-lisbon"
              :comment {:id "r3" :reply-to "1f0c9a52-6d3e-4b1a-9a7e-2c4d5e6f7a81" :name "Fay"
                        :moment "2026-10-29T11:00:00.000Z" :text "Text of r3."}}
             (last walk)))
      (is (nil? (get-in (first walk) [:comment :reply-to]))))))

(deftest age-test
  (let [now (java.time.Instant/parse "2026-10-29T12:00:00Z")]
    (testing "an age is said in the largest whole unit"
      (is (= "just now" (comments/age "2026-10-29T11:59:30.000Z" now)))
      (is (= "1 minute ago" (comments/age "2026-10-29T11:59:00.000Z" now)))
      (is (= "5 hours ago" (comments/age "2026-10-29T07:00:00.000Z" now)))
      (is (= "3 days ago" (comments/age "2026-10-26T11:00:00.000Z" now))))
    (testing "a moment that is no moment has no age"
      (is (nil? (comments/age "29-10-2026 12:00" now))))))

(deftest step-text-test
  (let [now   (java.time.Instant/parse "2026-10-29T12:00:00Z")
        found (comments/row-comment (row "r3" "a-week-in-lisbon" "1f0c9a52-6d3e-4b1a-9a7e-2c4d5e6f7a81"
                                         "2026-10-29T10:00:00.000Z"))]
    (testing "a step shows its place, the post, the name, the age, what it answers and the text"
      (is (= (str "2 of 3 \u00b7 a-week-in-lisbon \u00b7 Fay \u00b7 2 hours ago\n"
                  "In answer to Ann Lee: Tram 28 at dawn is the best advice in this post. We went at seven and had it **\u2026\n"
                  "Text of r3.")
             (comments/step-text lisbon now 2 3 found))))
    (testing "a step with a moment that is no moment leaves out the age"
      (is (str/starts-with? (comments/step-text lisbon now 1 1 (assoc-in found [:comment :moment] "x"))
                            "1 of 1 \u00b7 a-week-in-lisbon \u00b7 Fay\n")))))

(deftest decided-test
  (let [a     {:folder "x" :comment {:id "a"}}
        reply {:folder "x" :comment {:id "r" :reply-to "a"}}
        state (reduce comments/decided comments/nothing-decided
                      [[:approve a] [:reply a reply] [:decline a] [:skip a] [:skip a]])]
    (testing "each decision is kept, and a reply approves the comment it answers"
      (is (= {:approved [a a] :replies [reply] :declined 1 :skipped 2} state)))
    (testing "the comments carried are those of each post with the approved and the replies added"
      (is (= {"x" [{:id "old"} {:id "a"} {:id "r" :reply-to "a"}]}
             (comments/carried {"x" [{:id "old"}] "y" [{:id "other"}]}
                               (-> comments/nothing-decided
                                   (comments/decided [:reply a reply]))))))
    (testing "the summary counts each decision, and the comments dropped"
      (is (= "1 approved, 1 replied to, 1 declined, 2 skipped. Dropped: 2 for a missing post."
             (comments/summary [a] state {:missing-post ["p" "q"] :missing-comment []})))
      (is (= "No comments wait in the sheet. Dropped: 1 for a missing comment."
             (comments/summary [] comments/nothing-decided {:missing-post [] :missing-comment ["c"]}))))))

(deftest choice-test
  (testing "a choice is its letter, in either case, and no line at all is a stop"
    (is (= :approve (comments/choice " A ")))
    (is (= :stop (comments/choice "t")))
    (is (= :stop (comments/choice nil)))
    (is (nil? (comments/choice "x")))))

(defn- walked
  "What `walk-sheet!` says, gives, publishes and deletes, in a copy of the example blog whose
   published posts are its posts of Lisbon and Hello, for the rows `rows`, the lines the author
   types `lines`, the answer `yes?`, what the publish gives `gives`, and the ids `stuck` that
   cannot be deleted."
  [{:keys [rows lines yes? gives stuck edited] :or {yes? true gives 0 stuck #{}}}]
  (example/with-copy {:comments {:script "https://s" :on? true}}
    (fn [dir]
      (fs/create-dirs (fs/path dir "docs"))
      (spit (fs/file dir "docs" "site.edn")
            (pr-str {:posts [{:folder "a-week-in-lisbon"} {:folder "hello-bareblog"}]}))
      (let [said      (atom [])
            published (atom [])
            deleted   (atom [])
            typed     (atom lines)
            how       {:say               #(swap! said conj %)
                       :ask               (constantly yes?)
                       :prompt            (fn [_] (let [line (first @typed)] (swap! typed rest) line))
                       :edit!             (fn [text] (str edited text))
                       :publish-comments! #(do (swap! published conj %) gives)}
            delete!   (fn [id] (if (stuck id) false (do (swap! deleted conj id) true)))
            gave      (comments/walk-sheet! how {:dir dir} rows delete!)]
        {:gave gave :said @said :published @published :deleted @deleted}))))

(deftest walk-sheet-test
  (let [rows [(row "r1" "hello-bareblog" "" "2026-10-28T10:00:00.000Z")
              (row "r2" "a-week-in-lisbon" "" "2026-10-29T10:00:00.000Z")
              (row "r3" "a-week-in-lisbon" "1f0c9a52-6d3e-4b1a-9a7e-2c4d5e6f7a81" "2026-10-29T11:00:00.000Z")
              (row "r4" "a-week-in-lisbon" "" "2026-10-29T12:00:00.000Z")
              (row "r5" "gone" "" "2026-10-29T12:00:00.000Z")]]
    (testing "an approval, a decline, a reply and a skip: the approved are published, and their
              rows, the declined and the dropped are deleted"
      (let [{:keys [gave said published deleted]} (walked {:rows rows :lines ["a" "d" "r" "Thanks!" "s"]})
            [{carried :comments :keys [message]}] published]
        (is (= 0 gave))
        (is (= ["r5" "r2" "r1" "r3"] deleted))
        (is (= "Publish comments on a-week-in-lisbon, hello-bareblog" message))
        (is (= ["r1"] (map :id (carried "hello-bareblog"))))
        (let [[replied reply] (take-last 2 (carried "a-week-in-lisbon"))]
          (is (= "r3" (:id replied)))
          (is (= {:reply-to "r3" :author? true :text "Thanks!"} (select-keys reply [:reply-to :author? :text]))))
        (is (some #{"Your reply:\nThanks!"} said))
        (is (some #{"1 approved, 1 replied to, 1 declined, 1 skipped. Dropped: 1 for a missing post."} said))
        (testing "and each wait for the sheet is said before it"
          (is (= ["Deleting 1 row from the sheet\u2026" "Deleting its row\u2026"
                  "Deleting the rows of the approved comments\u2026"]
                 (filter #(str/starts-with? % "Deleting") said))))))
    (testing "a stop ends the walk-through, and publishes what was approved before it"
      (let [{:keys [published deleted]} (walked {:rows rows :lines ["a" "t"]})]
        (is (= {"hello-bareblog" ["r1"]} (update-vals (:comments (first published)) (partial map :id))))
        (is (= ["r5" "r1"] deleted))))
    (testing "a publish that fails deletes no row of an approved comment"
      (let [{:keys [gave deleted]} (walked {:rows rows :lines ["a" "t"] :gives 1})]
        (is (= 1 gave))
        (is (= ["r5"] deleted))))
    (testing "a decline that is not confirmed, or a reply that is empty, asks again"
      (let [{:keys [published deleted]} (walked {:rows (take 1 rows) :lines ["d" "r" "" "a"] :yes? false})]
        (is (= ["r1"] (map :id (get-in (first published) [:comments "hello-bareblog"]))))
        (is (= ["r1"] deleted))))
    (testing "a row that cannot be deleted is skipped, and said"
      (let [{:keys [published said]} (walked {:rows (take 1 rows) :lines ["d"] :stuck #{"r1"}})]
        (is (empty? published))
        (is (some #(re-find #"stays in the sheet" %) said))))
    (testing "no rows publish nothing"
      (let [{:keys [gave said published]} (walked {:rows []})]
        (is (= 0 gave))
        (is (= "No comments wait in the sheet." (last said)))
        (is (empty? published))))))
