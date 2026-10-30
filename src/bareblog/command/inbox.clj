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
   the browser, and `:publish-all!` that publishes every page and gives the number the process
   ends with. It gives that number too."
  (:require [babashka.fs :as fs]
            [babashka.http-client :as http]
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
  "The version of the script that this BareBlog holds."
  2)

;; The script. Pure.

(defn script-text
  "The text of the script, from `template`, with the version and the limits of a comment
   written in."
  [template]
  (reduce-kv (fn [text k v] (str/replace text (str "{{" (name k) "}}") (str v)))
             template
             (assoc wire/comment-limits :version script-version)))

;; What a line and an answer mean. Pure.

(def ^:private address-shape #"https://script\.google\.com/macros/s/[A-Za-z0-9_-]+/exec")

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
      {:wrong (str "Google refused it (403). Deploy the script of the copy of the sheet, with "
                   "access for Anyone.")}

      (= 404 status)
      {:wrong "Nothing answers there (404). Copy the address that the deploy shows."}

      :else
      {:wrong (str "That address answers with a page, not as the script (" status ").")})))

(defn older?
  "True when the script that gave `answered` is older than the one this BareBlog holds."
  [answered]
  (< (or (:version answered) 0) script-version))

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

(declare switch!)

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

(defn- newer-script!
  "Writes the script that this BareBlog holds to a file outside the blog, and says what to do
   with it."
  [{:keys [say]} home]
  (let [file (fs/create-temp-file {:prefix "bareblog-inbox-" :suffix ".gs"})]
    (spit (fs/file file) (read-script home))
    (say (str "The script of the sheet is older than this BareBlog's. The newer one is in " file
              ". In the sheet: Extensions, Apps Script, paste it over the code, save, and deploy "
              "a new version under Manage deployments."))))

(defn- check-answers!
  "What the script at `url` answers at each step of a check with comments `on?`, as
   `check-problem` takes them. Every step is taken, also after one that went wrong."
  [url on?]
  (let [state (author! url "state" {:open on?})
        sent  (ask-script! url test-comment)]
    (cond-> {:state state :sent sent}
      on? (assoc :rows    (author! url "rows" {})
                 :deleted (author! url "delete" {:id (get-in sent [:script :id])})))))

(defn check!
  "Checks the script of the blog in `dir`: it takes the state of the setting, a test comment as a
   page sends it, and gives it back and deletes it, or refuses it while comments are off."
  [{:keys [say] :as how} {:keys [dir home]}]
  (if-some [{:keys [script on?]} (read-setting dir)]
    (do
      (say "Checking the script\u2026")
      (let [answers (check-answers! script (boolean on?))]
        (if-some [problem (check-problem (boolean on?) answers)]
          (do (say (str "The check failed. " problem)) 1)
          (do (say (str "The script works, and comments are " (if on? "on" "off") "."))
              (when (older? (get-in answers [:state :script])) (newer-script! how home))
              0))))
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

(defn connect!
  "Connects the blog in `dir` to its script: at the address `asked`, or else at the one the
   author pastes, once the copy link of the template sheet is open. Writes the address to the
   settings, checks the script, and offers to turn comments on."
  [{:keys [say browse!] :as how} {:keys [dir] :as where} asked]
  (when-not asked
    (browse! template-link)
    (say (str "The template sheet is open in the browser: " template-link))
    (say (str "Make a copy. In the copy: Extensions, Apps Script, Deploy, New deployment, Web app, "
              "Execute as Me, access for Anyone. Approve it, and copy the web app address.")))
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
          :else
          (let [on? (boolean (:on? (read-setting dir)))]
            (write-setting! dir {:script url :on? on?})
            (say (str "Connected. The settings hold the address: " url))
            (let [checked (check! how where)]
              (if (or (pos? checked) on?)
                checked
                (switch! how where true)))))))
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
