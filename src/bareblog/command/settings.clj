(ns bareblog.command.settings
  "How a command changes the settings of a blog: one setting at a time, in the text, so that what
   the author wrote around it stays as it was.

   The changes are pure. Reading and writing the file are the edge, at the end."
  (:require [babashka.fs :as fs]
            [bareblog.blog.layout :as layout]
            [clojure.string :as str]
            [rewrite-clj.node :as node]
            [rewrite-clj.parser :as parser]
            [rewrite-clj.zip :as z]))

(defn- ends-line? [n]
  (contains? #{:comment :newline} (node/tag n)))

(defn- space? [n]
  (contains? #{:whitespace :newline :comma} (node/tag n)))

(defn- newline? [n]
  (= :newline (some-> n node/tag)))

(defn- added
  "The nodes of the map `nodes` with the setting `entry` at its end: on a line of its own
   where the map ends with one, and after the last setting where it ends on the line of it."
  [nodes entry]
  (let [body     (vec (reverse (drop-while space? (reverse nodes))))
        trailing (subvec (vec nodes) (count body))
        indent   (node/spaces 1)
        new-line (node/newlines 1)]
    (cond
      (empty? body)             (concat entry trailing)
      (ends-line? (peek body))  (concat body [indent] entry [new-line] trailing)
      (some newline? trailing)  (concat body [new-line indent] entry trailing)
      :else                     (concat body [indent] entry trailing))))

(defn- blank? [n]
  (= :whitespace (node/tag n)))

(defn- blanks
  "How many of `nodes` in a row from the first are spaces on a line."
  [nodes]
  (count (take-while blank? nodes)))

(defn- taken
  "The nodes of the map `nodes` without the setting whose key is at `i` and its value at `j`,
   and without what set it apart: the spaces before it, with the end of the line before it
   where that is a new line, or the spaces after it where it is the first, or the end of its
   own line after a comment."
  [nodes i j]
  (let [start  (- i (blanks (rseq (subvec nodes 0 i))))
        before (get nodes (dec start))
        after  (subvec nodes (inc j))
        spaces (blanks after)]
    (concat (subvec nodes 0 (cond-> start (newline? before) dec))
            (cond
              (nil? before)                         (drop spaces after)
              (and (= :comment (node/tag before))
                   (newline? (get after spaces)))  (drop (inc spaces) after)
              :else                                 after))))

(defn- setting? [n]
  (not (or (space? n) (= :comment (node/tag n)))))

(defn- key-of? [k n]
  (and (= :token (node/tag n)) (= k (node/sexpr n))))

(defn- index-of
  "The index of the first of `nodes` from `from` that `pred` holds for, or nil."
  [pred nodes from]
  (some (fn [i] (when (pred (nodes i)) i)) (range from (count nodes))))

(defn- setting-at
  "The index of the key `k` and of its value among the nodes of the map `nodes`, or nil."
  [k nodes]
  (when-some [i (index-of (partial key-of? k) nodes 0)]
    [i (index-of setting? nodes (inc i))]))

(defn- written
  "The text of `value` as the settings are written: a map with no commas between its entries."
  [value]
  (if (map? value)
    (str "{" (str/join " " (map (fn [[k v]] (str (written k) " " (written v))) value)) "}")
    (pr-str value)))

(defn- set-setting [k value nodes]
  (let [value-node (parser/parse-string (written value))]
    (if-some [[_ j] (setting-at k nodes)]
      (assoc nodes j value-node)
      (added nodes [(node/keyword-node k) (node/spaces 1) value-node]))))

(defn- take-setting [k nodes]
  (if-some [[i j] (setting-at k nodes)]
    (taken nodes i j)
    nodes))

(defn- rewritten
  "The text of the settings `text` with the nodes of its map as `change` makes them of those it
   has."
  [text change]
  (let [settings (z/of-string text)
        map-node (z/node settings)
        changed  (change (vec (node/children map-node)))]
    (z/root-string (z/replace settings (node/replace-children map-node changed)))))

(defn with-setting
  "The text of the settings `text` with `value` under the key `k`."
  [text k value]
  (rewritten text (partial set-setting k value)))

(defn without-setting
  "The text of the settings `text` with nothing under the key `k`."
  [text k]
  (rewritten text (partial take-setting k)))

(defn read-text
  "The text of the settings of the blog in the directory `dir`."
  [dir]
  (slurp (fs/file dir layout/settings-file)))

(defn write!
  "Writes `text` as the settings of the blog in the directory `dir`."
  [dir text]
  (spit (fs/file dir layout/settings-file) text))
