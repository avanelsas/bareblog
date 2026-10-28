(ns bareblog.app.style-test
  "Holds the stylesheet of the blog to the settings that it draws."
  (:require [bareblog.blog.site :as site]
            [clojure.test :refer [deftest is testing]]))

(def ^:private stylesheet
  "The stylesheet that every blog template shares."
  "resources/public/blog.css")

(defn- named-textures
  "The textures that the text `css` names in a selector, as text."
  [css]
  (set (map second (re-seq #"data-texture=\"(\w+)\"" css))))

(defn- drawn-textures
  "The textures that the text `css` gives a picture of their own, as text."
  [css]
  (set (map second (re-seq #"\[data-texture=\"(\w+)\"\] \{\s*background-size: \d+px;\s*background-image:"
                           css))))

(deftest texture-test
  (let [css  (slurp stylesheet)
        none (name site/default-texture)]
    (testing "every texture of the settings but none is drawn, and none is the default"
      (is (= (set (map name (remove #{site/default-texture} site/textures))) (drawn-textures css)))
      (is (some #{site/default-texture} site/textures)))
    (testing "the stylesheet names no texture that the settings do not have"
      (is (= (set (map name site/textures)) (named-textures css))))
    (testing "what every texture shares is said of the texture none, and of no other"
      (is (= #{none} (set (map second (re-seq #"data-texture=\"(\w+)\"\]\)" css))))))))
