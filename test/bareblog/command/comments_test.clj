(ns bareblog.command.comments-test
  (:require [bareblog.blog.wire :as wire]
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
