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

(def readers
  "How the text of each field that BareBlog reads from a header block is read, by the key of
   the field."
  {:title   identity
   :date    identity
   :tags    tags
   :summary identity})

(defn- named
  "The key and the text of the header line `line`, or nil when it is not a field."
  [line]
  (when-some [[_ field-name text] (re-matches #"\s*([A-Za-z][\w-]*)\s*:\s*(.*?)\s*" line)]
    [(keyword (str/lower-case field-name)) text]))

(defn- field
  "The key and the value of the header line `line`, or nil when BareBlog does not read it."
  [line]
  (let [[k text]  (named line)
        read-text (readers k)]
    (when (and read-text (not (str/blank? text)))
      [k (read-text text)])))

(defn fields
  "The fields of the header `lines`, by key. A line BareBlog does not read is left out."
  [lines]
  (into {} (keep field) lines))
