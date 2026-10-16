(ns bareblog.example
  "A directory to change in a test: one with nothing in it, or a copy of the example blog,
   with its settings, its posts, its drafts and its pages."
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]))

(def dir "example")

(defn- little-end [n]
  [(bit-and n 0xff) (bit-shift-right n 8)])

(defn write-gif!
  "Writes the start of a GIF of `width` by `height` pixels, which is all that is read of its
   size, as the file `file-name` in the directory `target`."
  [target file-name width height]
  (fs/write-bytes (fs/path target file-name)
                  (byte-array (map unchecked-byte (concat (map int "GIF89a") (little-end width) (little-end height))))))

(defn- copy-to!
  "Copies what an author wrote of the example blog into the directory `target`, with
   `settings` put over its settings."
  [target settings]
  (doseq [posts ["posts" "drafts" "pages"]]
    (fs/copy-tree (fs/path dir posts) (fs/path target posts)))
  (spit (fs/file target "blog.edn")
        (pr-str (merge (edn/read-string (slurp (fs/file dir "blog.edn"))) settings))))

(defn with-copy
  "What `check` gives for the directory of a copy of the example blog, with `settings` put
   over its settings. The copy is gone afterwards."
  [settings check]
  (let [target (fs/create-temp-dir)]
    (try
      (copy-to! target settings)
      (check (str target))
      (finally
        (fs/delete-tree target)))))

(defn with-empty-dir
  "What `check` gives for a directory with nothing in it. It is gone afterwards."
  [check]
  (let [dir (str (fs/create-temp-dir))]
    (try
      (check dir)
      (finally
        (fs/delete-tree dir)))))
