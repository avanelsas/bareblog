(ns bareblog.example-test
  "Holds the post of the example blog that shows every kind to the kinds there are."
  (:require [bareblog.app.blocks :as blocks]
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
