(ns bareblog.blog.comments
  "The comments of a post, as its `comments.edn` holds them.

   A comment is a map: its `:id`, the `:reply-to` id of the comment it answers or nil, the
   `:name` of the commenter, its `:moment` as a text of ISO 8601, its `:text` in Markdown, and
   `:author?` when the author wrote it. A comment of the author has no name.

       {:id \"6ae3815b-a62e-4d3a-b45c-bf7e58211f94\" :reply-to nil :name \"Ann\"
        :moment \"2026-10-29T08:51:36.104Z\" :text \"A first comment.\"}

   When it is read, its text is blocks, under `:blocks`."
  (:require [clojure.string :as str]))

(def short-id-length
  "How many characters of the id of a comment its short id is."
  8)

(defn short-id
  "The short id of the comment with the id `id`: its start, as a command takes it."
  [id]
  (subs id 0 (min short-id-length (count id))))

(defn- answers
  "The comments of `comments` by the id of the comment they answer, oldest first. A comment
   that answers none among them is under nil."
  [comments]
  (let [ids (set (map :id comments))]
    (group-by (comp ids :reply-to) (sort-by (juxt :moment :id) comments))))

(defn thread
  "The `comments` in the order a thread shows them, each with its `:depth`: oldest first, and
   each reply after the comment it answers, one deeper. A reply to a comment that is not among
   them stands at the top."
  [comments]
  (let [by-answered (answers comments)]
    (letfn [(with-replies [depth comment]
              (cons (assoc comment :depth depth)
                    (mapcat (partial with-replies (inc depth)) (by-answered (:id comment)))))]
      (vec (mapcat (partial with-replies 0) (by-answered nil))))))

(defn- text?
  "True when `x` is a text with more than spaces in it."
  [x]
  (and (string? x) (not (str/blank? x))))

(defn- whole?
  "True when `comment` holds what a comment holds."
  [{:keys [id moment text author?] commenter :name :as comment}]
  (and (map? comment) (text? id) (text? moment) (string? text)
       (or author? (text? commenter))))

(defn problems
  "What is wrong with `comments`, what the comments file of the post in `folder` holds, as
   texts for its author."
  [folder comments]
  (cond
    (not (vector? comments))
    [(str "The comments of " folder " are not a list.")]

    (not-every? whole? comments)
    [(str "A comment of " folder " has no id, moment, text or name.")]

    (not (apply distinct? nil (map :id comments)))
    [(str "Two comments of " folder " have the same id.")]

    (not= (count comments) (count (thread comments)))
    [(str "A comment of " folder " answers a comment that answers it.")]

    :else []))
