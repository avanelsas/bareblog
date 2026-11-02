(ns bareblog.command.inbox
  "The inbox of the comments of a blog: a Google Sheet of the author with BareBlog's script in
   it, deployed as a web app, connected and checked, and comments switched on and off in it.
   `bareblog.command.comments` reads and deletes its rows.

   The setting `:comments` of the blog holds the address of the script and whether comments are
   on:

       {:comments {:script \"https://script.google.com/macros/s/.../exec\" :on? true}}

   The secret that the command calls the script with is one file of the author, outside every
   blog. What an address and an answer mean, and what a check found, are pure, at the top. Each
   function at the end is given how, with `:say`, `:ask` that puts a question and gives true for
   a yes, `:prompt` that gives the line the author types, `:browse!` that opens an address in
   the browser, `:copy!` that puts a text on the clipboard and gives true when it did, and
   `:publish-all!` that publishes every page and gives the number the process ends with. It
   gives that number too."
  (:require [babashka.fs :as fs]
            [babashka.http-client :as http]
            [bareblog.blog.text :as text]
            [bareblog.blog.wire :as wire]
            [bareblog.command.settings :as settings]
            [bareblog.read.source :as source]
            [cheshire.core :as json]
            [clojure.string :as str]))

(def template-link
  "The link that copies the template sheet of BareBlog, with the script in it. `RELEASING.md`
   says how the template sheet is made."
  "https://docs.google.com/spreadsheets/d/1FimDdTs7SaNtW98cFFYnhU-MXXoqz8S9FcTs3zmTb8k/copy")

(def script-version
  "The version of the script that this BareBlog holds, as the template sheet has it."
  3)

(def script-needs
  "The least version of the script that this BareBlog works with. A sheet copied before keeps
   the script it has, and gets no feature of a newer one, such as notices."
  2)

(def ^:private notices-version
  "The first version of the script that sends notices."
  3)

(def notice-gap-ms
  "The least time between two emails that tell the author that comments wait."
  (* 60 60 1000))

;; The script. Pure.

(defn script-text
  "The text of the script, from `template`, with the version, the limits of a comment, the gap
   between two notices and the pattern of a folder written in."
  [template]
  (reduce-kv (fn [text k v] (str/replace text (str "{{" (name k) "}}") (str v)))
             template
             (assoc wire/comment-limits
                    :version        script-version
                    :notice-gap-ms  notice-gap-ms
                    :folder-pattern text/folder-pattern)))

;; What a line and an answer mean. Pure.

(def ^:private address-shape
  "The address of a web app, with the ID of its deployment."
  #"https://script\.google\.com/macros/s/([A-Za-z0-9_-]+)/exec")

(defn address
  "What the line `line` that the author typed is: `:stop` when it is empty, the `:address` of a
   web app, or what is `:wrong` with it."
  [line]
  (let [s (str/trim (or line ""))]
    (cond
      (str/blank? s)
      {:stop true}

      (re-matches address-shape s)
      {:address s}

      (str/ends-with? s "/dev")
      {:wrong (str "That is the test address, ending in /dev. Copy the web app address, ending in "
                   "/exec.")}

      (str/includes? s "docs.google.com")
      {:wrong "That is the address of the sheet. Copy the web app address that the deploy shows."}

      :else
      {:wrong (str "That is no web app address. It looks like "
                   "https://script.google.com/macros/s/.../exec")})))

(defn answer
  "What the answer of an address, its `:status` and its `:body`, says: the `:script` that
   answered, or what is `:wrong` with the address."
  [{:keys [status body unreachable]}]
  (let [parsed (try (json/parse-string body true) (catch Exception _ nil))]
    (cond
      unreachable
      {:wrong (str "The address could not be reached: " unreachable)}

      (and (map? parsed) (contains? parsed :ok))
      {:script parsed}

      (= 403 status)
      {:wrong (str "Google refused it (403). The deploy is not for Anyone, or the script lacks "
                   "your permission. Deploy the script of the copy of the sheet with access for "
                   "Anyone. Or in Apps Script click Run, tick Select all, and Continue.")}

      (= 404 status)
      {:wrong "Nothing answers there (404). Copy the address that the deploy shows."}

      :else
      {:wrong (str "That address answers with a page, not as the script (" status ").")})))

(defn- version-of
  "The version of the script that gave `answered`, which is 0 for one that names none."
  [answered]
  (or (:version answered) 0))

(defn older?
  "True when the script that gave `answered` is older than the one this BareBlog needs."
  [answered]
  (< (version-of answered) script-needs))

(defn notices-of
  "What the script that gave `answered` does of notices: `:none` for one too old to send them,
   `:off`, `:on`, or `:blocked` when they are on and the script lacks the permission to send."
  [answered]
  (cond
    (< (version-of answered) notices-version) :none
    (not (:notify answered))                  :off
    (:authorized answered)                    :on
    :else                                     :blocked))

(def notices-texts
  "What tells the author what the script does of notices, by what `notices-of` gives."
  {:none    (str "The script of this sheet sends no notices. A new copy of the template sheet "
                 "does, which bareblog comments connect sets up.")
   :off     "Email notices are off. Turn them on with: bareblog comments notify --on"
   :on      "Email notices are on: an email comes when a comment waits, at most one an hour."
   :blocked (str "Email notices are on, but the script may not send email. In Apps Script click "
                 "Run, tick Select all, and Continue.")})

;; What a check found. Pure. A check is a list of steps, and each gives what was wrong, or nil.

(defn state-problem
  "What is wrong when the script answered `answered` to being told that comments are `on?`."
  [on? answered]
  (cond
    (not (:ok answered))        (str "The script refused its state: " (:error answered) ".")
    (not= on? (:open answered)) "The script did not take its state."))

(defn sent-problem
  "What is wrong when the script answered `answered` to a test comment, with comments `on?`."
  [on? answered]
  (cond
    (and on? (= :full (wire/refusal (:error answered))))
    (str "The inbox is full: " (:waiting-max wire/comment-limits) " comments wait, and readers "
         "can send no more. Read them with: bareblog comments")

    (and on? (not (:ok answered)))
    (str "The script refused a test comment: " (:error answered) ".")

    (and on? (not (:id answered)))
    "The script took a test comment and gave it no id."

    (and (not on?) (:ok answered))
    "The script took a test comment while comments are off."))

(defn read-problem
  "What is wrong when the rows that the script gave in `answered` should hold the comment `id`."
  [id answered]
  (when-not (some #(= id (:id %)) (:rows answered))
    (str "The test comment did not reach the sheet. Check that the script is in the sheet it was "
         "copied with.")))

(defn deleted-problem
  "What is wrong when the script answered `answered` to deleting the test comment."
  [answered]
  (when-not (:deleted answered)
    "The script did not delete the test comment. Delete its row in the sheet by hand."))

(defn- judged
  "What `judge` finds wrong with the script that `answered`, or what was wrong with reaching it."
  [{:keys [wrong script]} judge]
  (or wrong (judge script)))

(defn check-problem
  "The first thing that is wrong in `answers`, what the script answered at each step of a check
   with comments `on?`: to its `:state`, to the test comment `:sent`, and with comments on, to
   the `:rows` and to the test comment `:deleted`. Nil when nothing is."
  [on? {:keys [state sent rows deleted]}]
  (or (judged state (partial state-problem on?))
      (judged sent (partial sent-problem on?))
      (when on?
        (or (judged rows (partial read-problem (get-in sent [:script :id])))
            (judged deleted deleted-problem)))))

(defn check-outcome
  "What a check with comments `on?` that got `answers` came to: what `:failed`, the version of
   an `:older` script that works, or that it `:works`, with what it does of `:notices`. Notices
   that are on and cannot be sent fail it."
  [on? answers]
  (let [answered (get-in answers [:state :script])
        sends    (notices-of answered)]
    (if-some [problem (check-problem on? answers)]
      {:failed problem}
      (cond
        (older? answered)  {:older (version-of answered)}
        (= :blocked sends) {:failed (:blocked notices-texts)}
        :else              {:works true :notices sends}))))

;; A script before notices refuses to be told of them, as an action it does not know, so what it
;; is goes before what it answered.
(defn notify-outcome
  "What the script answered `answered` to being told that it sends notices when `on?`, or to
   being asked whether it does when `on?` is nil: what it does of `:notices`, or what is
   `:wrong`."
  [on? answered]
  (let [sends (notices-of answered)]
    (cond
      (= :none sends)
      {:notices :none}

      (not (:ok answered))
      {:wrong (str "The script refused: " (:error answered) ".")}

      (and (some? on?) (not= on? (:notify answered)))
      {:wrong "The script did not take the notices."}

      :else
      {:notices sends})))

(def test-comment
  "What the check sends as a page would: open long enough, and for no post, so that one that is
   left behind is dropped as a missing post."
  (wire/comment-body {:name "BareBlog check"
                      :text "A test comment of bareblog comments check."
                      :open (+ 1000 (:open-min-ms wire/comment-limits))}))

;; Calling the script.

(def ^:private no-redirects
  (http/client (assoc http/default-client-opts :follow-redirects :never)))

;; Apps Script runs a call, then answers with a redirect to where its answer waits. The
;; redirect is followed by hand, with a GET, so that a call is never sent twice.
(defn- call!
  "What the script at `url` answered to `body`: its `:status` and `:body`, or why it could not
   be `:unreachable`."
  [url body]
  (try
    (let [first-answer (http/post url {:body    (json/generate-string body)
                                       :headers {"Content-Type" "text/plain"}
                                       :client  no-redirects
                                       :throw   false})]
      (if-some [location (get-in first-answer [:headers "location"])]
        (http/get location {:throw false})
        first-answer))
    (catch java.io.IOException problem
      {:unreachable (or (ex-message problem) (str (type problem)))})))

(defn- ask-script!
  "What the script at `url` says to `body`, as `answer` gives it."
  [url body]
  (answer (call! url body)))

;; The secret.

(def ^:private secret-file
  (fs/path (fs/home) ".bareblog" "comments-secret"))

(defn- secret!
  "The secret of the author, made the first time, in a file only its owner can read."
  []
  (if (fs/exists? secret-file)
    (str/trim (slurp (fs/file secret-file)))
    (let [secret (str (random-uuid))]
      (fs/create-dirs (fs/parent secret-file) {:posix-file-permissions "rwx------"})
      (spit (fs/file secret-file) secret)
      (fs/set-posix-file-permissions secret-file "rw-------")
      secret)))

(defn- author!
  "What the script at `url` says to the author's `action`, with what `more` adds to it."
  [url action more]
  (ask-script! url (merge {:action action :secret (secret!)} more)))

;; The setting.

(defn read-setting
  "The setting of comments of the blog in the directory `dir`, or nil."
  [dir]
  (:comments (source/read-settings dir)))

(defn- write-setting! [dir setting]
  (settings/write! dir (settings/with-setting (settings/read-text dir) :comments setting)))

;; Doing it.

(declare switch! offer-notices!)

(def ^:private not-connected
  "What tells the author that comments are not connected, and how to connect them."
  "Comments are not connected. Connect them with: bareblog comments connect")

(def ^:private another-secret
  "What tells the author that the script holds a secret that came first, and how to reset it."
  (str "Another secret came first. In the sheet: Extensions, Apps Script, Project Settings, "
       "Script Properties, delete secret, and connect again."))

(defn read-script
  "The script that the BareBlog in `home` holds, with its version and limits written in."
  [home]
  (script-text (slurp (fs/file home "resources" "comments" "inbox.gs"))))

;; A sheet can have more than one deployment, and only the one at the address of the blog counts.
;; A new deployment gets another address, so the author is told which one to give a new version.
(defn deployment
  "The start of the ID of the deployment at the address `url`, enough to tell it from another, or
   nil when `url` is no address of a web app."
  [url]
  (some->> url (re-matches address-shape) second (take 12) (apply str)))

(defn- deployment-name
  "The words that name the deployment at the address `url`."
  [url]
  (if-some [id (deployment url)]
    (str "the deployment whose ID starts with " id)
    "the deployment of the blog"))

(defn newer-script-text
  "What tells the author that the newer script is in `file`, and on the clipboard when
   `copied?`, and how to deploy it at `url`."
  [file url copied?]
  (str "The script of the sheet is older than this BareBlog needs. The newer one is in " file
       (when copied? ", and on the clipboard") ". In the sheet: Extensions, Apps Script, paste it "
       "over the code, and save. Then Deploy, Manage deployments, " (deployment-name url) ": its "
       "pencil, Version: New version, and Deploy. A new deployment would get another address."))

(defn outcome-text
  "What tells the author what a check of the script at `url` with comments `on?` came to,
   `outcome`."
  [url on? {:keys [failed older notices]}]
  (cond
    failed (str "The check failed. " failed)
    older  (str "The script is still version " older " at " (deployment-name url) ", and this "
                "BareBlog needs " script-needs ". Save the code in Apps Script, then give this "
                "deployment a new version. A new deployment gets another address, which the "
                "blog does not call.")
    :else  (str "The script works, and comments are " (if on? "on" "off") "."
                (when-not (= :none notices) (str " " (notices-texts notices))))))

(defn- newer-script!
  "Writes the script that this BareBlog holds to a file outside the blog, puts it on the
   clipboard, and says how to deploy it at `url`."
  [{:keys [say copy!]} home url]
  (let [file   (fs/create-temp-file {:prefix "bareblog-inbox-" :suffix ".gs"})
        script (read-script home)]
    (spit (fs/file file) script)
    (say (newer-script-text file url (copy! script)))))

(defn- check-answers!
  "What the script at `url` answers at each step of a check with comments `on?`, as
   `check-problem` takes them. Every step is taken, also after one that went wrong."
  [url on?]
  (let [state (author! url "state" {:open on?})
        sent  (ask-script! url test-comment)]
    (cond-> {:state state :sent sent}
      on? (assoc :rows    (author! url "rows" {})
                 :deleted (author! url "delete" {:id (get-in sent [:script :id])})))))

(defn- checked!
  "What a check of the script at `url` with comments `on?` came to, as `check-outcome` gives it,
   saying first that it checks."
  [{:keys [say]} url on?]
  (say "Checking the script\u2026")
  (check-outcome on? (check-answers! url on?)))

;; An update fails in steps that Google does not report: the code left unsaved, a new
;; deployment in place of a new version, or no yes to what the script asks for. So the script
;; is checked again until it is the newer one and works, and each check says what it found.
(defn- updated!
  "Checks the script at `url` with comments `on?` each time the author presses Enter, until it is
   the newer one and works."
  [{:keys [say prompt] :as how} url on?]
  (loop []
    (if (nil? (prompt "Press Enter to check again, or Ctrl-C to stop:"))
      (do (say "The script was not updated. Check it again with: bareblog comments check") 1)
      (let [outcome (checked! how url on?)]
        (say (outcome-text url on? outcome))
        (if (:works outcome) 0 (recur))))))

(defn check!
  "Checks the script of the blog in `dir`: it takes the state of the setting, a test comment as a
   page sends it, and gives it back and deletes it, or refuses it while comments are off. An
   older script that works is updated with the author, and checked again."
  [{:keys [say] :as how} {:keys [dir home]}]
  (if-some [{:keys [script on?]} (read-setting dir)]
    (let [on?     (boolean on?)
          outcome (checked! how script on?)]
      (if (:older outcome)
        (do (newer-script! how home script) (updated! how script on?))
        (do (say (outcome-text script on? outcome)) (if (:works outcome) 0 1))))
    (do (say not-connected) 1)))

(defn- address-problem!
  "What is wrong with the address `url`, as the script there answers, or nil."
  [{:keys [say]} url]
  (say "Trying the address\u2026")
  (:wrong (ask-script! url {:action "version"})))

(defn- reachable-address!
  "The address that `asked` or else the author gives, once it answers as the script. Asks
   again after a wrong one, where the author typed it. Nil when the author stops."
  [{:keys [say prompt] :as how} asked]
  (loop [line (or asked (prompt "The web app address, or nothing to stop:"))]
    (let [{:keys [stop wrong] typed :address} (address line)
          wrong (or wrong (when typed (address-problem! how typed)))]
      (cond
        stop        nil
        (not wrong) typed
        asked       (do (say wrong) nil)
        :else       (do (say wrong) (recur (prompt "The web app address, or nothing to stop:")))))))

(defn- connected!
  "Writes the address `url` of the script that took the secret to the settings of the blog in
   `dir`, checks the script, and offers to turn comments on and to send notices."
  [{:keys [say] :as how} {:keys [dir] :as where} url]
  (let [on? (boolean (:on? (read-setting dir)))]
    (write-setting! dir {:script url :on? on?})
    (say (str "Connected. The settings hold the address: " url))
    (let [checked (check! how where)]
      (if (pos? checked)
        checked
        (max (if on? 0 (switch! how where true))
             (offer-notices! how url))))))

(defn connect!
  "Connects the blog in `dir` to its script: at the address `asked`, or else at the one the
   author pastes, once the copy link of the template sheet is open. Writes the address to the
   settings, checks the script, and offers to turn comments on and to send notices."
  [{:keys [say browse!] :as how} where asked]
  (when-not asked
    (browse! template-link)
    (say (str "The template sheet is open in the browser: " template-link))
    (say (str "Make a copy. In the copy: Extensions, Apps Script, Deploy, New deployment, Web app, "
              "Execute as Me, access for Anyone. When Google asks, tick Select all, and Continue. "
              "Copy the web app address.")))
  (if-some [url (reachable-address! how asked)]
    (do
      (say "Connecting\u2026")
      (let [{:keys [wrong script]} (author! url "connect" {})]
        (cond
          wrong                (do (say wrong) 1)
          (not (:ok script))   (do (say (if (= "wrong secret" (:error script))
                                          another-secret
                                          (:error script)))
                                   1)
          :else                (connected! how where url))))
    (do (say "Nothing was changed.") 1)))

(defn- told-problem!
  "Tells the script at `script` that comments are `on?`, saying so first, and gives what was
   wrong, or nil."
  [{:keys [say]} script on?]
  (say "Telling the script\u2026")
  (judged (author! script "state" {:open on?}) (partial state-problem on?)))

;; Comments go on only once the script takes them, so that no page shows a form that the script
;; refuses. They go off on every page also when the script cannot be told, which the check
;; tells it later.
(defn switch!
  "Switches comments of the blog in `dir` on, when `on?`, or off: in the script, in the
   settings, and on every page."
  [{:keys [say ask publish-all!] :as how} {:keys [dir]} on?]
  (let [{:keys [script] :as setting} (read-setting dir)
        word (if on? "on" "off")]
    (cond
      (and on? (nil? script))
      (do (say not-connected) 1)

      (= on? (boolean (:on? setting)))
      (do (say (str "Comments are " word " already.")) 0)

      (not (ask (str "Turn comments " word ", and publish every page?")))
      (do (say "Nothing was changed.") 1)

      :else
      (let [problem (when script (told-problem! how script on?))]
        (if (and on? problem)
          (do (say (str "Nothing was changed. The script did not open: " problem)) 1)
          (do (when problem
                (say (str "The script is still open: " problem
                          " It is closed with: bareblog comments check")))
              (write-setting! dir (assoc setting :on? on?))
              (publish-all!)))))))

;; The notices. A page has nothing of them, so they live in the script alone.

(defn- notices!
  "What the script at `url` sends of notices, after it is told that it sends them when `on?`, or
   is only asked when `on?` is nil: what it does of `:notices`, or what is `:wrong`."
  [url on?]
  (let [{:keys [wrong script]} (author! url "notify" (if (nil? on?) {} {:on on?}))]
    (if wrong {:wrong wrong} (notify-outcome on? script))))

(defn- told-notices!
  "Tells the script at `url` to send notices when `on?`, or not, or only asks when `on?` is nil,
   and says what it sends."
  [{:keys [say]} url on?]
  (let [{:keys [wrong notices]} (notices! url on?)]
    (say (or wrong (notices-texts notices)))
    (if (or wrong (and (some? on?) (#{:none :blocked} notices))) 1 0)))

(defn notify!
  "Tells the script of the blog in `dir` to send notices when `on?`, or not, or says whether it
   does when `on?` is nil."
  [{:keys [say] :as how} {:keys [dir]} on?]
  (if-some [{:keys [script]} (read-setting dir)]
    (told-notices! how script on?)
    (do (say not-connected) 1)))

(defn- offer-notices!
  "Asks the author whether the script at `url` should send notices, when it can and does not
   yet."
  [{:keys [say ask] :as how} url]
  (let [{:keys [wrong notices]} (notices! url nil)]
    (cond
      wrong               (do (say wrong) 1)
      (not= :off notices) 0
      (ask "Email you when a comment waits, at most once an hour?")
      (told-notices! how url true)
      :else               (do (say (:off notices-texts)) 0))))

;; The rows, for the walk-through.

(defn rows!
  "The rows of the sheet of the script at `url`, under `:rows`, or what is `:wrong`."
  [url]
  (let [{:keys [wrong script]} (author! url "rows" {})]
    (cond
      wrong              {:wrong wrong}
      (not (:ok script)) {:wrong (str "The script refused the rows: " (:error script) ".")}
      :else              {:rows (:rows script)})))

(defn delete-row!
  "Deletes the row `id` of the sheet of the script at `url`. Gives true when it did."
  [url id]
  (true? (get-in (author! url "delete" {:id id}) [:script :deleted])))
