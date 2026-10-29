(ns bareblog.blog.wire-test
  (:require [bareblog.blog.wire :as wire]
            [clojure.test :refer [deftest is testing]]))

(deftest comment-body-test
  (testing "a comment is sent with the names that the script reads"
    (is (= {:post "a-post" :name "Ann" :text "Hi." :replyTo "c1" :trap "" :open 6000}
           (wire/comment-body {:post "a-post" :name "Ann" :text "Hi." :reply-to "c1" :trap "" :open 6000}))))
  (testing "a comment that answers none, under no post, with no hidden field, is sent with empty texts"
    (is (= {:post "" :name "Ann" :text "Hi." :replyTo "" :trap "" :open 6000}
           (wire/comment-body {:name "Ann" :text "Hi." :open 6000})))))
