(ns bareblog.publish.run
  "Publishes a blog: one post, or the whole of it.

   The edge of the command. What a publish does is `bareblog.publish.plan`, which is pure.
   Here the folders are looked at, the author is asked, the pages are generated, and what
   changed is committed and pushed. `publish!` is given how, as `bareblog.publish.generate`
   takes it, with `:ask`, which puts a question to the author and gives true for a yes. It
   gives what came of it: an `:outcome`, with the `:message` of the commit or `:why` nothing
   was published."
  (:require [babashka.fs :as fs]
            [bareblog.blog.site :as site]
            [bareblog.blog.layout :as layout]
            [bareblog.publish.generate :as generate]
            [bareblog.publish.git :as git]
            [bareblog.publish.plan :as plan]
            [bareblog.read.source :as source]
            [clojure.string :as str]))

(def done?
  "The outcomes of a publish that went as the author asked."
  #{:published :unchanged})

(def ^:private told
  "What the author is told of each outcome, from what came of the publish."
  {:published     (fn [{:keys [message]}] (str "Published: " message "."))
   :unchanged     (constantly "Nothing changed, so nothing was committed.")
   :declined      (constantly "Nothing was published.")
   :refused       :why
   :failed        (fn [{:keys [why]}] (str why "\nNothing was published."))
   :not-committed (fn [{:keys [why]}] (str "The pages are written, and git could not commit them. " why))
   :not-pushed    (fn [{:keys [message why]}]
                    (str "Committed as \"" message "\", and git could not push. " why
                         "\nPush it with: git push"))})

(defn- post-dir [dir folder]
  (fs/path dir layout/posts-dir folder))

(defn- draft-dir [dir folder]
  (fs/path dir layout/drafts-dir folder))

(defn- find-where
  "Where the folder `folder` is in the blog in the directory `dir`."
  [dir folder]
  {:in-posts?  (fs/directory? (post-dir dir folder))
   :in-drafts? (fs/directory? (draft-dir dir folder))
   :in-pages?  (fs/directory? (fs/path dir layout/pages-dir folder))})

(defn- reading
  "How the blog in the directory `dir` is read for the plan `plan`, with the settings
   `settings`."
  [dir settings plan]
  (merge (source/published dir settings) (select-keys plan [:read :as-published :dates])))

;; A post is edited when what a page would show of it is not what was published. Both are
;; read for the settings that were published, so that a change of the settings is no edit.
(defn- find-edited
  "The published posts of the blog in `dir`, among `folders`, whose text on disk is not what
   was published as `published`."
  [dir published folders]
  (let [disk (reading dir (:blog published) {:dates (site/dates published)})
        kept (assoc disk :as-published folders)]
    (into #{}
          (filter (fn [folder] (not= (source/read-shown-text disk folder) (source/read-shown-text kept folder))))
          folders)))

(defn- move!
  "Moves the directory `from` to the directory `to`."
  [from to]
  (fs/create-dirs (fs/parent to))
  (fs/move from to))

(defn- write-pages!
  "Generates `blog`, and says how many pages it has. Throws when a post that is read has
   something wrong with it."
  [{:keys [say] :as how} blog]
  (when-some [problems (seq (source/read-problems blog))]
    (throw (ex-info (str/join "\n" problems) {})))
  (say (str (count (generate/generate! how blog)) " pages.")))

;; A draft is moved to the posts before it is read, since that is where a published post is.
;; When nothing comes of the publish, whatever the reason, it is moved back.
(defn- written!
  "Writes the pages of the blog in `dir` for the plan `plan`. Gives nil when they are written,
   and what came of it when they are not."
  [how dir settings {:keys [move] :as plan}]
  (when move
    (move! (draft-dir dir move) (post-dir dir move)))
  (try
    (write-pages! how (reading dir settings plan))
    nil
    (catch Exception problem
      (when move
        (move! (post-dir dir move) (draft-dir dir move)))
      {:outcome :failed :why (ex-message problem)})))

(defn- committed!
  "Commits what the plan `plan` changed in the blog in `dir`, and pushes it. Gives what came
   of it."
  [dir {:keys [paths message]}]
  (let [{:keys [committed? problem]} (git/commit! dir paths message)
        pushed                       (when committed? (git/push! dir))]
    (cond
      problem           {:outcome :not-committed :why problem}
      (not committed?)  {:outcome :unchanged}
      (:problem pushed) {:outcome :not-pushed :message message :why (:problem pushed)}
      :else             {:outcome :published :message message})))

(defn- carry-out!
  "Does what the plan `plan` says for the blog in `dir` with the settings `settings`. Gives
   what came of it."
  [{:keys [say ask] :as how} dir settings {:keys [removed refuse] :as plan}]
  (cond
    refuse
    {:outcome :refused :why refuse}

    (and (seq removed) (not (ask (plan/removal-question removed))))
    {:outcome :declined}

    :else
    (do
      (when-not (:address settings)
        (say "The settings name no :address, so the blog is written for the top of its host."))
      (or (written! how dir settings plan)
          (committed! dir plan)))))

(defn- publish-one!
  [{:keys [say] :as how} dir {:keys [folder now]}]
  (let [published (source/read-published dir)
        on-disk   (source/read-settings dir)
        settings  (or (:blog published) on-disk)]
    (cond
      (nil? published)
      (say "The blog was never published, so this post is the only one in it. Publish the rest with: bareblog publish --all")

      (not= settings on-disk)
      (say (str layout/settings-file " is not as it was published. Publish it with: bareblog publish --all")))
    (carry-out! how dir settings (plan/one published folder (find-where dir folder) now))))

(defn- publish-all!
  [{:keys [ask] :as how} dir {:keys [now]}]
  (let [published (source/read-published dir)
        settings  (source/read-settings dir)
        on-disk   (source/list-folders (source/published dir settings))
        there     (into (:posts on-disk) (:pages on-disk))
        edited    (when published
                    (find-edited dir published
                                 (into #{} (comp (map :folder) (filter there))
                                       (concat (:posts published) (:pages published)))))
        held-back (if (and (seq edited) (not (ask (plan/edits-question edited)))) edited #{})]
    (carry-out! how dir settings (plan/all published on-disk held-back now))))

(defn- not-a-repository [dir]
  {:outcome :refused
   :why     (str dir " is not the top of a git repository. A blog is one, so that publishing commits "
                 "the blog and nothing else.")})

(defn publish!
  "Publishes what `request` names of the blog in the directory `dir`: the post in its
   `:folder`, or the whole blog with `:all?`. `:now` in it is the moment of the publish,
   as text. Says what came of it, and gives that."
  [{:keys [say] :as how} dir {:keys [all?] :as request}]
  (let [came (try
               (cond
                 (not (git/top? dir)) (not-a-repository dir)
                 all?                 (publish-all! how dir request)
                 :else                (publish-one! how dir request))
               (catch Exception problem
                 {:outcome :failed :why (ex-message problem)}))]
    (say ((told (:outcome came)) came))
    came))
