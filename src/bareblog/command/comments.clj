(ns bareblog.command.comments
  "The `bareblog comments` command, of the comments of a blog: the walk-through of those that
   wait in the inbox, and the replies and the removals of the author, each published. The inbox
   itself is `bareblog.command.inbox`.

   What is shown and decided is pure, at the top. Each function at the end is given how, with
   `:say`, `:ask` that puts a question and gives true for a yes, `:prompt` that gives the line
   the author types, `:edit!` that opens the editor on a text and gives what the author saved,
   and `:publish-comments!` that publishes the `:comments` of each post by its folder, with the
   commit `:message`, and gives the number the process ends with. Each function gives that
   number too."
  (:require [bareblog.blog.comments :as comments]
            [bareblog.blog.site :as site]
            [bareblog.command.inbox :as inbox]
            [bareblog.read.source :as source]
            [clojure.string :as str]))

;; What a comment, a reply and a removal show. Pure.

(defn- shown-comment
  "What the author is shown of the comment that `found` holds, under `:comment`, with the post
   in `:folder`."
  [{:keys [folder] {commenter :name :keys [moment text]} :comment}]
  (str (or commenter "You") ", " (site/day moment) ", on " folder ":\n" text))

(defn- match-text
  "What names the comment of `match` by its id and its post."
  [{:keys [folder comment]}]
  (str (:id comment) " on " folder))

(def cut-line
  "The line in the editor above which the author writes a reply. It and what follows are no part
   of the reply."
  "------------------------ write your reply above this line ------------------------")

(defn editor-text
  "What the editor opens with for a reply to the comment that `shown` shows."
  [shown]
  (str "\n\n" cut-line "\n" shown "\n"))

(defn reply-of
  "The reply in `saved`, what the author saved in the editor: what is above the cut line."
  [saved]
  (let [s (or saved "")]
    (str/trim (subs s 0 (or (str/index-of s cut-line) (count s))))))

(defn- comments-off
  "What tells the author that there is no `what` while comments are off."
  [what]
  (str "Comments are off, so there is no " what ". Turn them on with: bareblog comments --on"))

(defn- removal-question
  "What the author is asked before a comment of the post in `folder` goes, with the number of
   `replies` under it."
  [folder replies]
  (str "Remove it"
       (case replies 0 "" 1 " and its reply" (str " and its " replies " replies"))
       " from " folder "?"))

;; The walk-through. Pure.

;; A script before version 2 let the sheet read a comment such as +1 as a number, and its rows
;; may wait still, so each value is taken as text.
(defn row-comment
  "The comment that the row `row` of the sheet holds, as its `:folder` and its `:comment`."
  [row]
  (let [{:keys [id received post replyTo text] commenter :name} (update-vals row str)]
    {:folder  post
     :comment {:id       id
               :reply-to (not-empty replyTo)
               :name     commenter
               :moment   received
               :text     text}}))

(defn- fate
  "What becomes of `found`, a comment of the sheet, for the published comments of each published
   post in `by-folder`, whose ids are `published`."
  [by-folder published {:keys [folder] {:keys [id reply-to]} :comment}]
  (cond
    (published id)                                                     :published
    (not (contains? by-folder folder))                                 :missing-post
    (and reply-to (not-any? #{reply-to} (map :id (by-folder folder)))) :missing-comment
    :else                                                              :walk))

(defn- ids-of
  "The ids of the comments in `fates` under `k`."
  [fates k]
  (mapv (comp :id :comment) (get fates k)))

(defn triage
  "What becomes of the rows `rows` of the sheet, for the published comments of each published
   post in `by-folder`: the ids of those that are `:published` already, the ids `:dropped`, by
   `:missing-post` and `:missing-comment`, and the comments to `:walk` through, oldest first,
   each as its `:folder` and its `:comment`."
  [rows by-folder]
  (let [published (into #{} (comp (mapcat val) (map :id)) by-folder)
        fates     (group-by (partial fate by-folder published) (map row-comment rows))]
    {:published (ids-of fates :published)
     :dropped   {:missing-post    (ids-of fates :missing-post)
                 :missing-comment (ids-of fates :missing-comment)}
     :walk      (vec (sort-by (comp (juxt :moment :id) :comment) (:walk fates)))}))

(defn- counted
  "The number `n` of `unit`, as `1 day` or `3 days`."
  [n unit]
  (str n " " unit (when (not= 1 n) "s")))

(defn- instant
  "The instant that the text `moment` names in ISO 8601, or nil when it names none."
  [moment]
  (try (java.time.Instant/parse moment) (catch java.time.format.DateTimeParseException _ nil)))

(defn age
  "How long before the instant `now` the moment `moment` was, as the author reads it, or nil
   when `moment` is no moment."
  [moment now]
  (when-some [then (instant moment)]
    (let [minutes (.toMinutes (java.time.Duration/between then now))]
      (cond
        (< minutes 1)    "just now"
        (< minutes 60)   (str (counted minutes "minute") " ago")
        (< minutes 1440) (str (counted (quot minutes 60) "hour") " ago")
        :else            (str (counted (quot minutes 1440) "day") " ago")))))

(defn- gist
  "The first line of `text`, cut to 80 characters."
  [text]
  (let [line (first (str/split-lines text))]
    (if (< 80 (count line)) (str (subs line 0 79) "\u2026") line)))

(defn step-text
  "What the author is shown of `found`, the comment `place` of `total` in the walk-through, at
   the instant `now`, with the comment it answers among the published comments in `by-folder`."
  [by-folder now place total
   {:keys [folder] {commenter :name :keys [moment text reply-to]} :comment}]
  (let [answered (first (filter (comp #{reply-to} :id) (by-folder folder)))]
    (str (str/join " \u00b7 " (remove nil? [(str place " of " total) folder commenter
                                            (age moment now)]))
         "\n"
         (when answered
           (str "In answer to " (or (:name answered) "you") ": " (gist (:text answered)) "\n"))
         text)))

(def nothing-decided
  "What is decided before the first comment of a walk-through."
  {:approved [] :replies [] :declined 0 :skipped 0})

(defn decided
  "What is decided after `state`, with the decision `decision`: `[:approve found]`, `[:reply
   found reply]`, `[:decline found]` or `[:skip found]`, each `found` and `reply` as its
   `:folder` and its `:comment`."
  [state [choice found reply]]
  (case choice
    :approve (update state :approved conj found)
    :reply   (-> state (update :approved conj found) (update :replies conj reply))
    :decline (update state :declined inc)
    :skip    (update state :skipped inc)))

(defn- with-added [carried {:keys [folder comment]}]
  (update carried folder conj comment))

(defn carried
  "The comments of each post that `state` adds to, by its folder: those it has in `by-folder`,
   with the approved comments and the replies."
  [by-folder {:keys [approved replies]}]
  (let [added (concat approved replies)]
    (reduce with-added (select-keys by-folder (map :folder added)) added)))

(defn- dropped-text
  "What the author is told of the comments `dropped`, or nothing when there are none."
  [{:keys [missing-post missing-comment]}]
  (let [counts (cond-> []
                 (seq missing-post)    (conj (str (count missing-post) " for a missing post"))
                 (seq missing-comment) (conj (str (count missing-comment)
                                                  " for a missing comment")))]
    (when (seq counts) (str " Dropped: " (str/join ", " counts) "."))))

(defn summary
  "What the author is told at the end of a walk-through of `walk`, of what `state` decided, and
   of the comments `dropped`."
  [walk {:keys [approved replies declined skipped]} dropped]
  (str (if (empty? walk)
         "No comments wait in the sheet."
         (str (- (count approved) (count replies)) " approved, " (count replies) " replied to, "
              declined " declined, " skipped " skipped."))
       (dropped-text dropped)))

(def ^:private choices
  "What the author types for each choice of a step."
  {"a" :approve "d" :decline "r" :reply "s" :skip "t" :stop})

(def ^:private choices-question
  "[a]pprove, [d]ecline, [r]eply, [s]kip or s[t]op?")

(defn choice
  "The choice that the line `line` the author typed is, or nil. No line at all is a stop."
  [line]
  (if (nil? line) :stop (choices (str/lower-case (str/trim line)))))

;; Finding a comment, and writing a reply.

(defn- read-published-comments
  "The comments of each published post of the blog in `dir`, by its folder, and what is wrong
   with them, as texts."
  [dir]
  (let [read (into {} (map (juxt :folder (comp (partial source/read-post-comments dir) :folder)))
                   (:posts (source/read-published dir)))]
    {:by-folder (update-vals read :comments)
     :problems  (mapcat :problems (vals read))}))

(defn- now-moment []
  (str (.truncatedTo (java.time.Instant/now) java.time.temporal.ChronoUnit/MILLIS)))

(defn- author-reply!
  "The reply of the author with the text `text` to `comment`, under a new id, at this moment."
  [comment text]
  (comments/reply comment text (str (random-uuid)) (now-moment)))

(defn- written!
  "The reply that the author gave as `given`, or else, when that is blank, what they write in
   the editor below the comment shown as `shown`."
  [edit! shown given]
  (if (str/blank? given) (reply-of (edit! (editor-text shown))) (str/trim given)))

(defn- read-found-comment
  "The published comment of the blog in `dir` whose id starts with `short`, as its `:folder`, its
   `:comment` and the `:comments` of its post, or what is `:wrong`."
  [dir short]
  (let [{:keys [by-folder problems]}                      (read-published-comments dir)
        [{:keys [folder] :as found} & more :as matches] (comments/matching short by-folder)]
    (cond
      (seq problems) {:wrong (str/join "\n" problems)}
      (nil? found)   {:wrong (str "No published comment has an id that starts with " short ".")}
      more           {:wrong (str "More than one comment has an id that starts with " short ": "
                                  (str/join ", " (map match-text matches))
                                  ". Give more of the id.")}
      :else          (assoc found :comments (by-folder folder)))))

;; Replying.

(defn- reply-to-found!
  "Shows the comment that `found` holds, takes the reply `text` or else what the author writes in
   the editor, asks, and publishes it."
  [{:keys [say ask edit! publish-comments!]} {:keys [folder comment comments] :as found} text]
  (let [shown (shown-comment found)]
    (say shown)
    (let [written (written! edit! shown text)]
      (cond
        (str/blank? written)
        (do (say "The reply is empty, so nothing was published.") 1)

        (not (ask (str "Your reply:\n" written "\nPublish it on " folder "?")))
        (do (say "Nothing was published.") 1)

        :else
        (publish-comments! {:comments {folder (conj comments (author-reply! comment written))}
                            :message  (str "Reply to a comment on " folder)})))))

(defn reply!
  "Publishes the reply `text` of the author, or else what the author writes in the editor, to
   the published comment of the blog in `dir` whose id starts with `short`."
  [{:keys [say] :as how} {:keys [dir]} short text]
  (if-not (:on? (inbox/read-setting dir))
    (do (say (comments-off "reply")) 1)
    (let [{:keys [wrong] :as found} (read-found-comment dir short)]
      (if wrong
        (do (say wrong) 1)
        (reply-to-found! how found text)))))

;; Removing.

(defn- remove-found!
  "Shows the comment that `found` holds, asks whether it goes with the replies under it, and
   publishes the post without them."
  [{:keys [say ask publish-comments!]} {:keys [folder comment comments] :as found}]
  (let [gone (comments/under comments (:id comment))]
    (say (shown-comment found))
    (if (ask (removal-question folder (dec (count gone))))
      (publish-comments! {:comments {folder (comments/without comments gone)}
                          :message  (str "Remove a comment on " folder)})
      (do (say "Nothing was published.") 1))))

(defn remove!
  "Removes the published comment of the blog in `dir` whose id starts with `short`, and every
   reply under it, and publishes its post."
  [{:keys [say] :as how} {:keys [dir]} short]
  (let [{:keys [wrong] :as found} (read-found-comment dir short)]
    (if wrong
      (do (say wrong) 1)
      (remove-found! how found))))

;; Walking through.

(defn- declined!
  "The decision for `found` once the author declined it: its row is deleted with `delete!`, or
   it is skipped when it cannot be."
  [{:keys [say]} delete! found]
  (say "Deleting its row\u2026")
  (if (delete! (get-in found [:comment :id]))
    [:decline found]
    (do (say "Its row could not be deleted, so it stays in the sheet.") [:skip found])))

(defn- reply-written!
  "The reply of the author to `found`, shown as `shown`: the line they type, or else what they
   write in the editor. Nil when it is empty."
  [{:keys [say prompt edit!]} shown {:keys [folder comment]}]
  (let [written (written! edit! shown (prompt "Your reply, or nothing to write it in the editor:"))]
    (when-not (str/blank? written)
      (say (str "Your reply:\n" written))
      {:folder folder :comment (author-reply! comment written)})))

(defn- decision!
  "What the author decides of `found`, shown as `shown`, with `delete!` that deletes a row."
  [{:keys [ask prompt] :as how} delete! shown found]
  (loop []
    (case (choice (prompt choices-question))
      :approve [:approve found]
      :skip    [:skip found]
      :stop    [:stop]
      :decline (if (ask "Decline it? Its row is deleted, and that cannot be undone.")
                 (declined! how delete! found)
                 (recur))
      :reply   (if-some [reply (reply-written! how shown found)]
                 [:reply found reply]
                 (recur))
      (recur))))

(defn- walked!
  "What the author decides of the comments `walk`, one at a time, until the last or a stop."
  [{:keys [say] :as how} delete! by-folder walk]
  (let [now (java.time.Instant/now)]
    (loop [state nothing-decided [found & more] walk place 1]
      (if (nil? found)
        state
        (let [shown (step-text by-folder now place (count walk) found)]
          (say shown)
          (let [decision (decision! how delete! shown found)]
            (if (= :stop (first decision))
              state
              (recur (decided state decision) more (inc place)))))))))

(defn- delete-all!
  "Deletes the rows `ids` with `delete!`, saying `doing` first, and says when some could not be."
  [{:keys [say]} delete! doing ids]
  (when (seq ids)
    (say doing)
    (when-some [kept (seq (remove delete! ids))]
      (say (str (counted (count kept) "row") " could not be deleted. The next walk-through deletes "
                "them.")))))

(defn- publish-decided!
  "Publishes what `state` approved, among the published comments in `by-folder`, and deletes
   the rows of the approved comments with `delete!` once that succeeded. Gives the number the
   process ends with."
  [{:keys [publish-comments!] :as how} delete! by-folder {:keys [approved] :as state}]
  (if (empty? approved)
    0
    (let [publish (carried by-folder state)
          folders (str/join ", " (sort (keys publish)))
          gave    (publish-comments! {:comments publish
                                      :message  (str "Publish comments on " folders)})]
      (when (zero? gave)
        (delete-all! how delete! "Deleting the rows of the approved comments\u2026"
                     (map (comp :id :comment) approved)))
      gave)))

(defn walk-sheet!
  "Walks the author through the rows `rows` of the sheet of the blog in `dir`, with `delete!`
   that deletes a row and gives true when it did, and publishes what was approved. A row is
   deleted once it is dropped, declined, or published."
  [{:keys [say] :as how} {:keys [dir]} rows delete!]
  (let [{:keys [by-folder problems]}     (read-published-comments dir)
        {:keys [published dropped walk]} (triage rows by-folder)
        gone                             (concat published (mapcat val dropped))]
    (if (seq problems)
      (do (say (str/join "\n" problems)) 1)
      (do
        (delete-all! how delete!
                     (str "Deleting " (counted (count gone) "row") " from the sheet\u2026")
                     gone)
        (let [state (walked! how delete! by-folder walk)]
          (say (summary walk state dropped))
          (publish-decided! how delete! by-folder state))))))

(defn walk!
  "Walks the author through the comments in the sheet of the blog in `dir`, and publishes what
   was approved."
  [{:keys [say] :as how} {:keys [dir] :as where}]
  (let [{:keys [script on?]} (inbox/read-setting dir)]
    (if-not on?
      (do (say (comments-off "walk-through")) 1)
      (do
        (say "Reading the comments in the sheet\u2026")
        (let [{:keys [wrong rows]} (inbox/rows! script)]
          (if wrong
            (do (say wrong) 1)
            (walk-sheet! how where rows (partial inbox/delete-row! script))))))))
