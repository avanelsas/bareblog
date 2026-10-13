(ns bareblog.blog.shape-test
  (:require [bareblog.blog.shape :as shape]
            [clojure.test :refer [deftest is testing]]))

(deftest named-test
  (testing "the default blog template has a shape"
    (is (some? (shape/named shape/default-template))))
  (testing "every shape says what the home page shows, where a choice takes effect and how a reader searches"
    (doseq [[template-name {:keys [home choice search] :as named}] shape/by-name]
      (is (= #{:home :choice :search} (set (keys named))) template-name)
      (is (contains? #{:entries :newest-post} home) template-name)
      (is (contains? #{:home :everywhere} choice) template-name)
      (is (contains? #{:field :palette} search) template-name)))
  (testing "a name with no blog template is refused, with the name"
    (is (= {:template :poster}
           (ex-data (try (shape/named :poster) (catch #?(:clj Exception :cljs :default) e e)))))))

(deftest of-blog-test
  (testing "the shape of a blog is the one of the blog template its settings name, or of the default"
    (is (= (shape/named :notebook) (shape/of-blog {:template :notebook})))
    (is (= (shape/named shape/default-template) (shape/of-blog {})))
    (is (= [:notebook shape/default-template]
           [(shape/template-name {:template :notebook}) (shape/template-name {})]))))

(deftest chooses-on-test
  (testing "a shape says on which kinds of page a choice of the reader shows"
    (is (shape/chooses-on? (shape/named :journal) :home))
    (is (not (shape/chooses-on? (shape/named :journal) :post)))
    (is (shape/chooses-on? (shape/named :notebook) :post))
    (is (not (shape/chooses-on? (shape/named :notebook) nil)))))

(deftest home-pages-test
  (let [site {:blog {:per-page 2} :posts [{:folder "a"} {:folder "b"} {:folder "c"}]}]
    (testing "a home page of entries has as many pages as the entries fill"
      (is (= 2 (shape/home-pages (shape/named :journal) site))))
    (testing "a home page that shows a post is one page"
      (is (= 1 (shape/home-pages (shape/named :notebook) site))))))
