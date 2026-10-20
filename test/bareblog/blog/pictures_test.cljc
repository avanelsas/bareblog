(ns bareblog.blog.pictures-test
  (:require [bareblog.blog.pictures :as pictures]
            [clojure.test :refer [deftest is testing]]))

(deftest named-test
  (testing "settings that name no picture name none"
    (is (empty? (pictures/named {:title "Bare notes"}))))
  (testing "a link preview is one picture"
    (is (= [{:at [:link-preview] :src "card.jpg"}] (pictures/named {:link-preview "card.jpg"}))))
  (testing "one name for a logo or a favicon is its picture in the light mode, which both modes show"
    (is (= [{:at [:logo :light] :src "logo.svg"}] (pictures/named {:logo "logo.svg"}))))
  (testing "a map names a picture for each mode"
    (is (= #{{:at [:favicon :light] :src "icon.svg"} {:at [:favicon :dark] :src "icon-dark.svg"}}
           (set (pictures/named {:favicon {:light "icon.svg" :dark "icon-dark.svg"}})))))
  (testing "the pictures stand in the order of the kinds"
    (is (= [:link-preview :logo :favicon]
           (map pictures/kind-of (pictures/named {:favicon "i.svg" :logo "l.svg" :link-preview "c.jpg"})))))
  (testing "a map that names no picture for the light mode names none"
    (is (empty? (pictures/named {:logo {:dark "logo-dark.svg"}})))))

(deftest fits-test
  (testing "a picture fits its kind by the extension of its name, whatever its case"
    (is (pictures/fits? {:at [:logo :light] :src "logo.SVG"}))
    (is (pictures/fits? {:at [:favicon :dark] :src "icon.ico"}))
    (is (pictures/fits? {:at [:link-preview] :src "card.jpeg"})))
  (testing "a format that its kind does not take does not fit"
    (is (not (pictures/fits? {:at [:link-preview] :src "card.svg"})))
    (is (not (pictures/fits? {:at [:favicon :light] :src "icon.webp"}))))
  (testing "a name that leads elsewhere, has no extension, or is no text does not fit"
    (is (not (pictures/fits? {:at [:logo :light] :src "../logo.svg"})))
    (is (not (pictures/fits? {:at [:logo :light] :src "svg"})))
    (is (not (pictures/fits? {:at [:logo :light] :src 5})))))

(deftest settings-problems-test
  (testing "a name, or a map with a picture for the light mode, is no problem"
    (is (empty? (pictures/settings-problems {:logo "logo.svg" :favicon {:light "i.svg" :dark "d.svg"}}))))
  (testing "anything else, for a kind with a picture for each mode, is"
    (is (= 2 (count (pictures/settings-problems {:logo {:dark "d.svg"} :favicon {:light "i.svg" :dim "x.svg"}}))))
    (is (= ["blog.edn gives 5 as its :logo, which is the name of a picture, or a map with one under :light and maybe one under :dark."]
           (pictures/settings-problems {:logo 5})))))

(deftest files-test
  (testing "the files of the pictures of a blog are each named once"
    (is (= #{"logo.svg" "card.jpg"}
           (set (pictures/files {:logo         {:light {:src "logo.svg" :version "a"} :dark {:src "logo.svg" :version "a"}}
                                 :link-preview {:src "card.jpg" :version "b" :width 400 :height 400}}))))
    (is (empty? (pictures/files nil)))))
