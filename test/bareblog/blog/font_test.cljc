(ns bareblog.blog.font-test
  (:require [bareblog.blog.font :as font]
            [bareblog.blog.site :as site]
            [clojure.test :refer [deftest is testing]]))

(deftest family-test
  (testing "the settings name a family by its name"
    (is (= "Literata" (font/family {:font "Literata"})))
    (is (= "Source Serif 4" (font/family {:font "Source Serif 4"}))))
  (testing "a blog that names none, or names one wrong, has none"
    (is (nil? (font/family {})))
    (is (nil? (font/family {:font :literata})))
    (is (nil? (font/family {:font "Bad\"name"})))
    (is (nil? (font/family {:font " Literata"})))))

(deftest settings-problems-test
  (testing "a font that is no name is a problem of the settings"
    (is (= ["blog.edn gives :literata as its :font, which is the name of a font in quotes, of letters, digits, spaces, dots, dashes and underscores."]
           (font/settings-problems {:font :literata})))
    (is (= (font/settings-problems {:font ""}) (site/settings-problems {:font ""}))))
  (testing "a name, or no font, is none"
    (is (empty? (font/settings-problems {:font "Iowan Old Style"})))
    (is (empty? (font/settings-problems {})))))

(deftest folder-test
  (testing "a family has a folder among the fonts, named by its letters and digits"
    (is (= "literata" (font/slug "Literata")))
    (is (= "source-serif-4" (font/slug "Source Serif 4")))
    (is (= "fonts/m-plus-1p" (font/folder "M  PLUS 1p ")))))

(deftest located-test
  (testing "a file of the folder is named from the top of the blog"
    (is (= "src: url(fonts/a/x.woff2) format('woff2');"
           (font/located "src: url(x.woff2) format('woff2');" "fonts/a")))
    (is (= "url(\"fonts/a/x.woff2\") url('fonts/a/y.woff')"
           (font/located "url(\"x.woff2\") url( 'y.woff' )" "fonts/a"))))
  (testing "a file named from a host, from the top or as data is left as it is"
    (doseq [css ["url(https://example.com/x.woff2)" "url(/x.woff2)" "url(data:font/woff2;base64,AA)"]]
      (is (= css (font/located css "fonts/a"))))))

(deftest stylesheet-test
  (testing "a family with faces has them, and names itself as the font of the blog"
    (is (= "@font-face { src: url(fonts/literata/a.woff2); }\n:root {\n  --font: \"Literata\";\n}\n"
           (font/stylesheet "Literata" "@font-face { src: url(a.woff2); }"))))
  (testing "a family without faces only names itself, for a reader who has it installed"
    (is (= ":root {\n  --font: \"Georgia\";\n}\n" (font/stylesheet "Georgia" nil))))
  (testing "a blog with no family has an empty stylesheet"
    (is (= "" (font/stylesheet nil "@font-face {}")))))
