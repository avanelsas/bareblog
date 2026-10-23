(ns bareblog.command.tags
  "The `bareblog tags` command: the tags of a blog, and a tag renamed in every post and page.

   The edge of the command. Which tags are one tag, and whether a rename may be done, is
   `bareblog.blog.tags`, which is pure. Each function here is given how, with `:say`, and
   `:ask` that puts a question to the author and gives true for a yes. It gives the number that
   the process ends with."
  (:require [bareblog.blog.tags :as tags]
            [bareblog.read.reader :as reader]
            [bareblog.read.source :as source]
            [bareblog.written.output :as output]
            [clojure.string :as str]))

(defn- read-tags
  "The text of `found` with what it holds and its tags."
  [{:keys [file] :as found}]
  (let [text (slurp file)]
    (assoc found :text text :tags (:tags (reader/read-post text)))))

(defn- read-tagged
  "Each text of the blog in the directory `dir`, with what it holds and its tags."
  [dir]
  (map read-tags (source/list-texts dir)))

(defn- counted
  "A tag with how many texts have it, as a line."
  [[tag-name n]]
  (str tag-name " (" n ")"))

(defn list!
  "Says each tag of the blog in the directory `dir`, with how many posts and pages have it, and
   each tag of the posts that is written in more than one way, as a publish says it."
  [{:keys [say]} dir]
  (let [texts  (read-tagged dir)
        counts (frequencies (mapcat :tags texts))]
    (if (empty? counts)
      (say "No post or page has a tag.")
      (do (run! (comp say counted) (sort-by (juxt (comp tags/folded key) key) counts))
          (run! say (tags/problems (remove :page? texts) #{}))))
    0))

(defn- tag-line
  "The tags `tag-names` as an author writes them."
  [tag-names]
  (str/join " " (map reader/written-tag tag-names)))

(defn- change
  "What renaming the tag `from` as `to` changes of the text `found`: its new tags, and the text
   with them, which is nil when it cannot be written. Nil when it has not the tag."
  [from to {:keys [text] :as found}]
  (when (some #{from} (:tags found))
    (let [new-tags (tags/renamed (:tags found) from to)]
      (assoc found :new-tags new-tags :written (reader/retagged text new-tags)))))

(defn- unfit
  "Why `to` cannot be a tag, or nil when it can."
  [to]
  (when (or (not (reader/tag? to)) (output/pageless-tag? to))
    (str (pr-str to) " cannot be a tag. A tag has no # in it and no slash, and is not dots alone.")))

(defn rename-plan
  "What renaming the tag `from` as `to` does to the texts `texts`: why it is refused under
   `:refusal`, the places of the texts whose tags cannot be written anew under `:stuck`, or the
   `:changes`."
  [texts from to]
  (let [changes (keep (partial change from to) texts)
        stuck   (remove :written changes)]
    (if-some [refusal (or (unfit to) (tags/rename-refusal (mapcat :tags texts) from to))]
      {:refusal refusal}
      (if (seq stuck)
        {:stuck (mapv :place stuck)}
        {:changes changes}))))

(defn- published-folders
  "The folders of the posts and the pages of the blog in `dir` that were published."
  [dir]
  (let [published (source/read-published dir)]
    (into #{} (map :folder) (concat (:posts published) (:pages published)))))

(defn- tell-published!
  "Tells the author which of the `changes` were published, and how to publish them again."
  [{:keys [say]} dir changes]
  (let [published (published-folders dir)
        again     (for [{:keys [folder draft?]} changes
                        :when                   (and (not draft?) (published folder))]
                    folder)]
    (when (seq again)
      (say (str "Published before, and changed now: " (str/join ", " again) ". "
                "Publish them together with: bareblog publish --all")))))

(defn- written!
  "Writes the texts of the `changes`, and says what came of it."
  [{:keys [say] :as how} dir changes]
  (doseq [{:keys [file written]} changes]
    (spit file written))
  (say (str "Renamed in " (count changes) (if (next changes) " texts." " text.")))
  (tell-published! how dir changes)
  0)

(defn- show!
  "Says the tags of each of the `changes`, as they are and as they become."
  [{:keys [say]} changes]
  (doseq [{:keys [place new-tags] :as found} changes]
    (say (str place ": " (tag-line (:tags found))))
    (say (str (str/join (repeat (count place) " ")) "  becomes " (tag-line new-tags)))))

(defn rename!
  "Renames the tag `from` as `to` in every post, draft and page of the blog in the directory
   `dir`, once the author says yes to what changes."
  [{:keys [say ask] :as how} dir from to]
  (let [from                            (reader/tag-named from)
        to                              (reader/tag-named to)
        {:keys [refusal stuck changes]} (rename-plan (read-tagged dir) from to)]
    (cond
      refusal
      (do (say refusal) 1)

      stuck
      (do (say (str "The tags of " (str/join ", " stuck) " cannot be written anew. Nothing was changed."))
          1)

      :else
      (do (show! how changes)
          (if (ask (str "Rename " (pr-str from) " as " (pr-str to) "?"))
            (written! how dir changes)
            (do (say "Nothing was changed.") 1))))))
