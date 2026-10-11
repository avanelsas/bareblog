(ns bareblog.header
  "The header block of a post: the lines between two lines of three dashes at its top."
  (:require [clojure.string :as str]))

(def fence "---")

(defn- fence? [line]
  (= fence (str/trimr line)))

(defn split
  "The header lines and the body of the post `text`. A text with no header block has no header
   lines, and all of it is the body."
  [text]
  (let [[opening & more] (str/split-lines text)
        [inside closing] (split-with (complement fence?) more)]
    (if (and opening (fence? opening) (seq closing))
      {:lines (vec inside) :body (str/join "\n" (rest closing))}
      {:lines [] :body text})))

(defn- tags
  "The tags in `text`, which names them with commas between."
  [text]
  (into [] (comp (map str/trim) (remove str/blank?)) (str/split text #",")))

(defn- date
  "The date in `text`, when it is written with the year first, as in 2026-10-11. A time may
   follow it. Nil for anything else."
  [text]
  (when (re-matches #"\d{4}-\d{2}-\d{2}([T ].*)?" text)
    text))

(def readers
  "How the text of each field that BareBlog reads from a header block is read, by the key of
   the field. A reader gives nil for text it cannot read."
  {:title   identity
   :date    date
   :tags    tags
   :summary identity})

(defn- named
  "The key and the text of the header line `line`, or nil when it is not a field."
  [line]
  (when-some [[_ field-name text] (re-matches #"\s*([A-Za-z][\w-]*)\s*:\s*(.*?)\s*" line)]
    [(keyword (str/lower-case field-name)) text]))

(defn- read-field
  "What the header line `line` is: the key of its field, its text, and its value when the text
   could be read. Nil when BareBlog does not read the line."
  [line]
  (let [[k text]  (named line)
        read-text (readers k)]
    (when (and read-text (not (str/blank? text)))
      {:key k :text text :value (read-text text)})))

(defn- field [{:keys [key value]}]
  (when (some? value)
    [key value]))

(defn fields
  "The fields of the header `lines`, by key. A line that BareBlog does not read, or whose text
   it cannot read, is left out."
  [lines]
  (into {} (comp (keep read-field) (keep field)) lines))

(defn- problem [{:keys [key text value]}]
  (when (nil? value)
    (str "The " (name key) " \"" text "\" could not be read. Write a date as 2026-10-11.")))

(defn problems
  "What is wrong with the header `lines`, as a text for each field that could not be read."
  [lines]
  (into [] (comp (keep read-field) (keep problem)) lines))
