(ns bareblog.example
  "A copy of the example blog to change in a test: its settings, its posts and its drafts."
  (:require [babashka.fs :as fs]
            [clojure.edn :as edn]))

(def dir "example")

(defn- copy-to!
  "Copies what an author wrote of the example blog into the directory `target`, with
   `settings` put over its settings."
  [target settings]
  (doseq [posts ["posts" "drafts"]]
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
