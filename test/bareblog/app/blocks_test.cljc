(ns bareblog.app.blocks-test
  (:require [bareblog.app.blocks :as blocks]
            [baremirror.alpha.template :as template]
            [clojure.test :refer [deftest is testing]]))

(defn all-fixed?
  "True when `x` is text or a number, or a fixed template whose children are too."
  [x]
  (or (string? x)
      (number? x)
      (nil? x)
      (and (vector? x)
           (template/fixed? x)
           (every? all-fixed? (last (template/pieces x))))))

(def examples
  "One block of each kind."
  {:text    {:kind    :text
             :content ["We took the " {:kind :strong :content ["tram 28"]} " to "
                       {:kind :link :href "https://example.com" :title "A hill" :content ["Alfama"]}
                       ", " {:kind :emphasis :content ["slowly"]} ", with " {:kind :code :content ["bb"]}
                       " and " {:kind :image :src "a.png" :alt "a map"} "."]}
   :heading {:kind :heading :level 2 :content ["The " {:kind :emphasis :content ["old"]} " town"]}
   :photo   {:kind :photo :src "harbour.svg" :alt "The harbour at dawn"}
   :unknown {:kind :unknown :type :blockquote :text "A quote."}})

(deftest template-test
  (testing "a text block is a paragraph, and each thing it holds is an element of its kind"
    (is (= [:p "We took the " [:strong "tram 28"] " to "
            [:a {:href "https://example.com" :title "A hill"} "Alfama"]
            ", " [:em "slowly"] ", with " [:code "bb"]
            " and " [:img {:src "a.png" :alt "a map"}] "."]
           (blocks/template (:text examples)))))
  (testing "a link with no title has no title attribute"
    (is (= [:p [:a {:href "/x"} "x"]]
           (blocks/template {:kind :text :content [{:kind :link :href "/x" :content ["x"]}]}))))
  (testing "a heading block is a heading of its level"
    (is (= [:h2 "The " [:em "old"] " town"] (blocks/template (:heading examples)))))
  (testing "a photo block is an x-image in a figure"
    (is (= [:figure [:x-image {:src "harbour.svg" :alt "The harbour at dawn"}]]
           (blocks/template (:photo examples)))))
  (testing "an unknown block is a paragraph with its plain text"
    (is (= [:p "A quote."] (blocks/template (:unknown examples))))))

(deftest every-kind-test
  (testing "there is an example of every kind of block"
    (is (= (set (keys blocks/templates)) (set (keys examples)))))
  (testing "the example of a text block holds every kind of content"
    (is (= (set (keys blocks/inlines))
           (set (keep :kind (:content (:text examples)))))))
  (testing "the template of every kind is fixed all the way down"
    (is (every? all-fixed? (map blocks/template (vals examples))))))

(deftest no-template-test
  (testing "a block of a kind with no template is refused, with its kind"
    (is (= {:kind :video}
           (ex-data (try (blocks/template {:kind :video}) (catch #?(:clj Exception :cljs :default) e e))))))
  (testing "content of a kind with no template is refused, with its kind"
    (is (= {:kind :footnote}
           (ex-data (try (blocks/template {:kind :text :content [{:kind :footnote}]})
                         (catch #?(:clj Exception :cljs :default) e e)))))))
