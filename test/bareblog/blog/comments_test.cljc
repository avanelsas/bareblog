(ns bareblog.blog.comments-test
  (:require [bareblog.blog.comments :as comments]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(defn- said
  "A comment with the id `id`, sent at the moment `moment`, that answers `reply-to`."
  [id moment reply-to]
  {:id id :reply-to reply-to :name "Ann" :moment moment :text "Text."})

(deftest thread-test
  (testing "comments stand oldest first, each reply after the comment it answers, one deeper"
    (is (= [["a" 0] ["c" 1] ["d" 2] ["b" 0] ["e" 1]]
           (map (juxt :id :depth)
                (comments/thread [(said "b" "2026-10-02" nil)
                                  (said "d" "2026-10-04" "c")
                                  (said "e" "2026-10-05" "b")
                                  (said "a" "2026-10-01" nil)
                                  (said "c" "2026-10-03" "a")])))))
  (testing "comments of the same moment stand by their id"
    (is (= ["a" "b"] (map :id (comments/thread [(said "b" "2026-10-01" nil) (said "a" "2026-10-01" nil)])))))
  (testing "a reply to a comment that is not there stands at the top"
    (is (= [["a" 0]] (map (juxt :id :depth) (comments/thread [(said "a" "2026-10-01" "gone")]))))))

(deftest short-id-test
  (testing "a short id is the start of the id"
    (is (= "6ae3815b" (comments/short-id "6ae3815b-a62e-4d3a-b45c-bf7e58211f94"))))
  (testing "a short id is never longer than the id"
    (is (= "a1" (comments/short-id "a1")))))

(deftest problems-test
  (let [a (said "a" "2026-10-01" nil)]
    (testing "comments that are whole have no problem"
      (is (= [] (comments/problems "posts/x" [])))
      (is (= [] (comments/problems "posts/x" [a {:id "b" :moment "2026-10-02" :text "Hi." :author? true}]))))
    (testing "comments that are not a list, are not whole, or share an id are a problem"
      (is (= ["The comments of posts/x are not a list."] (comments/problems "posts/x" {})))
      (is (= ["A comment of posts/x has no id, moment, text or name."]
             (comments/problems "posts/x" [(dissoc a :name)])))
      (is (= ["A comment of posts/x has no id, moment, text or name."]
             (comments/problems "posts/x" [(assoc a :name "  ")])))
      (is (= ["Two comments of posts/x have the same id."] (comments/problems "posts/x" [a a]))))
    (testing "comments that answer each other, which no thread reaches, are a problem"
      (is (= ["A comment of posts/x answers a comment that answers it."]
             (comments/problems "posts/x" [a (said "b" "2026-10-02" "c") (said "c" "2026-10-03" "b")]))))))

(deftest matching-test
  (let [a (said "1f0c9a52-6d3e" "2026-10-01" nil)
        b (said "1f0c9a99-0000" "2026-10-02" nil)]
    (testing "a comment is found by the start of its id, with the folder of its post"
      (is (= [{:folder "x" :comment a}] (comments/matching "1f0c9a52" {"x" [a] "y" [b]}))))
    (testing "a start that more comments have finds them all, and one that none has finds none"
      (is (= 2 (count (comments/matching "1f0c9a" {"x" [a] "y" [b]}))))
      (is (empty? (comments/matching "ffff" {"x" [a]}))))))

(deftest reply-test
  (testing "a reply of the author answers the comment, with no name"
    (is (= {:id "r" :reply-to "a" :author? true :moment "2026-10-29T10:00:00.000Z" :text "Thanks."}
           (comments/reply (said "a" "2026-10-01" nil) "Thanks." "r" "2026-10-29T10:00:00.000Z")))))

(deftest written-test
  (let [comments [(said "b" "2026-10-02" "a")
                  {:id "c" :reply-to "a" :author? true :moment "2026-10-02" :text "Hi \"you\"."}
                  (said "a" "2026-10-01" nil)]
        text     (comments/written comments)]
    (testing "the comments file holds the comments by moment, then by id"
      (is (= ["a" "b" "c"] (map :id (edn/read-string text))))
      (is (= (set comments) (set (edn/read-string text)))))
    (testing "it holds one comment to a line, each with its id first"
      (is (= 3 (count (str/split-lines text))))
      (is (str/starts-with? text "[{:id \"a\" :reply-to nil :name \"Ann\""))
      (is (str/ends-with? text "}]\n")))))

(deftest under-test
  (let [thread [(said "a" "2026-10-01" nil) (said "b" "2026-10-02" "a") (said "c" "2026-10-03" "b")
                (said "d" "2026-10-04" "a") (said "e" "2026-10-05" nil)]]
    (testing "a comment goes with every reply under it, at any depth, and with nothing else"
      (is (= #{"a" "b" "c" "d"} (comments/under thread "a")))
      (is (= #{"b" "c"} (comments/under thread "b"))))
    (testing "a comment with no replies goes alone"
      (is (= #{"e"} (comments/under thread "e"))))
    (testing "the comments without those ids are the rest, in their order"
      (is (= ["a" "d" "e"] (map :id (comments/without thread #{"b" "c"}))))
      (is (= [] (comments/without [(said "e" "2026-10-05" nil)] #{"e"}))))))
