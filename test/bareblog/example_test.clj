(ns bareblog.example-test
  "Holds the post of the example blog that shows every kind to the kinds there are, and the
   directory of a test to going when the test is done."
  (:require [babashka.fs :as fs]
            [bareblog.app.blocks :as blocks]
            [bareblog.example :as example]
            [bareblog.read.reader :as reader]
            [clojure.test :refer [deftest is testing]]))

(def ^:private post "example/posts/every-kind/post.md")

(defn- read-kinds
  "The kinds of everything in the post, at any depth."
  []
  (into #{} (keep :kind) (tree-seq coll? seq (:blocks (reader/read-post (slurp post))))))

(deftest the-post-holds-every-kind
  (let [kinds (read-kinds)]
    (testing "every kind of block that has a template, but the unknown one"
      (is (= #{} (reduce disj (set (keys blocks/templates)) (conj kinds :unknown)))))
    (testing "every kind of thing inside a block that has a template, but the unknown one"
      (is (= #{} (reduce disj (set (keys blocks/inlines)) (conj kinds :unknown)))))
    (testing "nothing that BareBlog has no kind for"
      (is (not (contains? kinds :unknown))))))

(defn- keep-writing!
  "Writes a file in the directory `dir` and removes the one before it every few milliseconds,
   for `ms` milliseconds, as git does when it keeps its house in a process of its own."
  [dir ms]
  (let [until   (+ (System/currentTimeMillis) ms)
        objects (fs/path dir ".git" "objects")]
    (fs/create-dirs objects)
    (loop [n 0]
      (when (< (System/currentTimeMillis) until)
        (try
          (spit (fs/file objects (str n)) "x")
          (fs/delete-if-exists (fs/path objects (str (dec n))))
          (catch java.io.IOException _))
        (Thread/sleep 2)
        (recur (inc n))))))

(deftest empty-dir-test
  (testing "the directory of a test goes, also while something still writes in it"
    (let [seen   (atom nil)
          writer (atom nil)]
      (example/with-empty-dir
        (fn [dir]
          (reset! seen dir)
          (reset! writer (future (keep-writing! dir 300)))
          (Thread/sleep 50)))
      @@writer
      (is (not (fs/exists? @seen))))))
