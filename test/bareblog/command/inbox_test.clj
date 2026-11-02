(ns bareblog.command.inbox-test
  (:require [bareblog.app.state :as state]
            [bareblog.blog.wire :as wire]
            [bareblog.command.inbox :as inbox]
            [bareblog.example :as example]
            [bareblog.read.source :as source]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(deftest script-text-test
  (let [text (inbox/script-text (slurp "resources/comments/inbox.gs"))]
    (testing "the script holds the version and the limits of this BareBlog, and no placeholder"
      (is (str/includes? text (str "var VERSION = " inbox/script-version ";")))
      (is (str/includes? text (str "var TEXT_MAX = " (:text-max wire/comment-limits) ";")))
      (is (str/includes? text (str "var OPEN_MIN_MS = " (:open-min-ms wire/comment-limits) ";")))
      (is (str/includes? text (str "var WAITING_MAX = " (:waiting-max wire/comment-limits) ";")))
      (is (str/includes? text (str "var NOTICE_GAP_MS = " inbox/notice-gap-ms ";")))
      (is (str/includes? text "var FOLDER = /^(?:[a-z0-9][a-z0-9-]*)?$/;"))
      (is (not (str/includes? text "{{"))))))

(deftest refusals-test
  (let [text (inbox/script-text (slurp "resources/comments/inbox.gs"))]
    (testing "each refusal that the page and the command read is one that the script gives"
      (is (every? #(str/includes? text (str "error: '" %)) (keys wire/refusals))))
    (testing "the page tells a reader the kind of a refusal, and anything else as a failure"
      (is (= :full (:error (state/outcome "Hi." {:ok false :error "the inbox is full"}))))
      (is (= :failed (:error (state/outcome "Hi." {:ok false :error "Exception: Sheets"})))))))

(deftest address-test
  (testing "the address of a web app is taken, with the spaces around it"
    (is (= {:address "https://script.google.com/macros/s/AKfy_c-1/exec"}
           (inbox/address "  https://script.google.com/macros/s/AKfy_c-1/exec \n"))))
  (testing "nothing stops"
    (is (= {:stop true} (inbox/address "")))
    (is (= {:stop true} (inbox/address nil))))
  (testing "the test address, the address of the sheet and anything else are each told apart"
    (is (re-find #"/dev" (:wrong (inbox/address "https://script.google.com/macros/s/AKfy/dev"))))
    (is (re-find #"of the sheet" (:wrong (inbox/address "https://docs.google.com/spreadsheets/d/1x/edit"))))
    (is (re-find #"no web app" (:wrong (inbox/address "http://script.google.com/macros/s/AKfy/exec"))))))

(deftest answer-test
  (testing "an answer in JSON with ok is the script"
    (is (= {:script {:ok false :error "wrong secret" :version 1}}
           (inbox/answer {:status 200 :body "{\"ok\":false,\"error\":\"wrong secret\",\"version\":1}"}))))
  (testing "a refusal, nothing, a page and no answer each say what is wrong"
    (is (re-find #"403" (:wrong (inbox/answer {:status 403 :body "<html>"}))))
    (is (re-find #"404" (:wrong (inbox/answer {:status 404 :body "<html>"}))))
    (is (re-find #"a page" (:wrong (inbox/answer {:status 200 :body "<html>"}))))
    (is (re-find #"not be reached" (:wrong (inbox/answer {:unreachable "timed out"}))))))

(deftest newer-script-text-test
  (let [url "https://script.google.com/macros/s/AKfycbxJdhUNq4c-RbDHZ7IQ/exec"]
    (testing "the deployment is named by the start of its ID, and an address of no web app names none"
      (is (= "AKfycbxJdhUN" (inbox/deployment url)))
      (is (nil? (inbox/deployment "https://example.com/")))
      (is (nil? (inbox/deployment nil))))
    (testing "the author is told where the script is, and which deployment gets a new version"
      (is (re-find #"in /tmp/a\.gs\. .*whose ID starts with AKfycbxJdhUN: its pencil"
                   (inbox/newer-script-text "/tmp/a.gs" url false)))
      (is (re-find #"in /tmp/a\.gs, and on the clipboard\. "
                   (inbox/newer-script-text "/tmp/a.gs" url true)))
      (is (re-find #"Manage deployments, the deployment of the blog: its pencil"
                   (inbox/newer-script-text "/tmp/a.gs" nil false))))
    (testing "a check says that the script works, why it failed, or at which deployment it is still older"
      (is (= "The script works, and comments are on. Email notices are on: an email comes when a comment waits, at most one an hour."
             (inbox/outcome-text url true {:works true :notices :on})))
      (is (re-find #"comments are off\. Email notices are off\. Turn them on with: bareblog comments notify --on"
                   (inbox/outcome-text url false {:works true :notices :off})))
      (is (= "The script works, and comments are on." (inbox/outcome-text url true {:works true :notices :none})))
      (is (= "The check failed. Nothing answers there."
             (inbox/outcome-text url true {:failed "Nothing answers there."})))
      (is (re-find #"still version 1 at the deployment whose ID starts with AKfycbxJdhUN, and this BareBlog needs 2"
                   (inbox/outcome-text url true {:older 1}))))))

(deftest check-outcome-test
  (let [state   (fn [version] {:script {:ok true :open false :version version}})
        refused {:script {:ok false :error "comments are closed"}}]
    (testing "a check that works is older by the version of the script"
      (is (= {:works true :notices :off} (inbox/check-outcome false {:state (state inbox/script-version) :sent refused})))
      (is (= {:works true :notices :none} (inbox/check-outcome false {:state (state inbox/script-needs) :sent refused})))
      (is (= {:older 1} (inbox/check-outcome false {:state (state 1) :sent refused})))
      (is (= {:older 0} (inbox/check-outcome false {:state (state nil) :sent refused}))))
    (testing "a check that failed is failed, also for an older script"
      (is (re-find #"while comments are off"
                   (:failed (inbox/check-outcome false {:state (state 1) :sent {:script {:ok true :id "a"}}})))))))

(defn- updating
  "What `check!` says and gives in a copy of the example blog, when the script answers each check
   with the next of `versions`, and the author presses Enter `presses` times before the input
   ends."
  [versions presses]
  (example/with-copy {:comments {:script "https://script.google.com/macros/s/AKfycbxJdhUNq/exec"}}
    (fn [dir]
      (let [said     (atom [])
            versions (atom versions)
            presses  (atom presses)
            answers  (fn [_url _on?]
                       (let [version (first @versions)]
                         (swap! versions rest)
                         {:state {:script {:ok true :open false :version version}}
                          :sent  {:script {:ok false :error "comments are closed"}}}))
            how      {:say    #(swap! said conj %)
                      :copy!  (constantly true)
                      :prompt (fn [_] (when (pos? @presses) (swap! presses dec) ""))}
            gave     (with-redefs-fn {#'inbox/check-answers! answers}
                       #(inbox/check! how {:dir dir :home "."}))]
        {:gave gave :said @said}))))

(deftest check-update-test
  (testing "a script that is up to date is checked once"
    (let [{:keys [gave said]} (updating [inbox/script-version] 0)]
      (is (= 0 gave))
      (is (= ["Checking the script…" "The script works, and comments are off. Email notices are off. Turn them on with: bareblog comments notify --on"] said))))
  (testing "an older script is checked again on each Enter, until it is the newer one"
    (let [{:keys [gave said]} (updating [1 1 inbox/script-version] 2)]
      (is (= 0 gave))
      (is (some #(re-find #"on the clipboard" %) said))
      (is (some #(re-find #"still version 1 at the deployment whose ID starts with AKfycbxJdhUN" %) said))
      (is (= "The script works, and comments are off. Email notices are off. Turn them on with: bareblog comments notify --on" (last said)))))
  (testing "an update that the author stops says how to check again"
    (let [{:keys [gave said]} (updating [1] 0)]
      (is (= 1 gave))
      (is (re-find #"not updated.*bareblog comments check" (last said))))))

(deftest notices-test
  (let [answered {:ok true :version inbox/script-version :notify true :authorized true}]
    (testing "a script sends notices when they are on and it may, and one before them sends none"
      (is (= :on (inbox/notices-of answered)))
      (is (= :off (inbox/notices-of (assoc answered :notify false))))
      (is (= :blocked (inbox/notices-of (assoc answered :authorized false))))
      (is (= :none (inbox/notices-of {:ok false :version 2 :error "unknown action"}))))
    (testing "notices that are on and may not be sent fail a check, with how to give the permission"
      (is (re-find #"tick Select all"
                   (:failed (inbox/check-outcome false {:state {:script (assoc answered :open false :authorized false)}
                                                        :sent  {:script {:ok false :error "comments are closed"}}})))))
    (testing "a script that took the notices, or was only asked, gives what it does of them"
      (is (= {:notices :on} (inbox/notify-outcome true answered)))
      (is (= {:notices :off} (inbox/notify-outcome nil (assoc answered :notify false)))))
    (testing "a script before notices refuses them as an unknown action, and sends none"
      (is (= {:notices :none} (inbox/notify-outcome true {:ok false :version 2 :error "unknown action"}))))
    (testing "a refusal and a script that did not take them each say so"
      (is (re-find #"refused: wrong secret" (:wrong (inbox/notify-outcome true (assoc answered :ok false :error "wrong secret")))))
      (is (re-find #"did not take" (:wrong (inbox/notify-outcome false answered)))))))

(deftest manifest-test
  (testing "the script asks for the sheet it is in, the address of its owner and email, and no more"
    (is (= ["https://www.googleapis.com/auth/spreadsheets.currentonly"
            "https://www.googleapis.com/auth/userinfo.email"
            "https://www.googleapis.com/auth/script.send_mail"]
           (:oauthScopes (json/parse-string (slurp "resources/comments/appsscript.json") true))))))

(deftest older-test
  (testing "a script below the version this BareBlog needs, or with none, is older"
    (is (inbox/older? {:version (dec inbox/script-needs)}))
    (is (inbox/older? {}))
    (is (not (inbox/older? {:version inbox/script-needs})))))

(deftest check-test
  (testing "the script takes its state, or says why not"
    (is (nil? (inbox/state-problem true {:ok true :open true})))
    (is (re-find #"did not take" (inbox/state-problem true {:ok true :open false})))
    (is (re-find #"wrong secret" (inbox/state-problem false {:ok false :error "wrong secret"}))))
  (testing "with comments on, a test comment is taken with an id, and with comments off it is refused"
    (is (nil? (inbox/sent-problem true {:ok true :id "a"})))
    (is (re-find #"no id" (inbox/sent-problem true {:ok true})))
    (is (re-find #"refused" (inbox/sent-problem true {:ok false :error "comments are closed"})))
    (is (re-find #"The inbox is full: 100 comments wait.*bareblog comments"
                 (inbox/sent-problem true {:ok false :error "the inbox is full"})))
    (is (nil? (inbox/sent-problem false {:ok false :error "comments are closed"})))
    (is (re-find #"while comments are off" (inbox/sent-problem false {:ok true :id "a"}))))
  (testing "the test comment is in the rows, and is deleted"
    (is (nil? (inbox/read-problem "a" {:rows [{:id "b"} {:id "a"}]})))
    (is (re-find #"did not reach" (inbox/read-problem "a" {:rows [{:id "b"}]})))
    (is (nil? (inbox/deleted-problem {:deleted true})))
    (is (re-find #"by hand" (inbox/deleted-problem {:deleted false}))))
  (testing "the test comment is open long enough, names no post, and is no trap"
    (is (< (:open-min-ms wire/comment-limits) (:open inbox/test-comment)))
    (is (= "" (:post inbox/test-comment) (:trap inbox/test-comment)))))

(deftest check-problem-test
  (let [ok      {:script {:ok true :open true :version 1}}
        sent    {:script {:ok true :id "t"}}
        rows    {:script {:ok true :rows [{:id "t"}]}}
        deleted {:script {:ok true :deleted true}}
        on      {:state ok :sent sent :rows rows :deleted deleted}]
    (testing "a check with comments on finds nothing when every step went well"
      (is (nil? (inbox/check-problem true on))))
    (testing "a check gives the first thing that went wrong, in the order of its steps"
      (is (re-find #"could not be reached" (inbox/check-problem true (assoc on :state {:wrong "The address could not be reached: x"}))))
      (is (re-find #"did not reach" (inbox/check-problem true (assoc on :rows {:script {:ok true :rows []}}))))
      (is (re-find #"by hand" (inbox/check-problem true (assoc on :deleted {:script {:ok true :deleted false}})))))
    (testing "a check with comments off wants the test comment refused, and reads no rows"
      (is (nil? (inbox/check-problem false {:state {:script {:ok true :open false}}
                                               :sent  {:script {:ok false :error "comments are closed"}}})))
      (is (re-find #"while comments are off"
                   (inbox/check-problem false {:state {:script {:ok true :open false}} :sent sent}))))))

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
            gave      (inbox/switch! how {:dir dir} on?)]
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
              gave    (inbox/connect! how {:dir dir} nil)]
          (is (= 1 gave))
          (is (= [inbox/template-link] @opened))
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
          (is (= 1 (inbox/connect! how {:dir dir} "https://docs.google.com/x")))
          (is (re-find #"address of the sheet" (first @said)))
          (is (= "Nothing was changed." (last @said))))))))
