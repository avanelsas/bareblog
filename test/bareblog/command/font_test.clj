(ns bareblog.command.font-test
  (:require [babashka.fs :as fs]
            [bareblog.command.font :as font]
            [bareblog.command.start :as start]
            [bareblog.example :as example]
            [bareblog.read.source :as source]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def ^:private catalogue
  "A catalogue of Google Fonts as it is sent, with three families."
  (str ")]}'\n"
       (json/generate-string
        {:familyMetadataList
         [{:family "Literata"
           :fonts  {:200 {} :200i {} :400 {} :400i {} :900 {} :900i {}}
           :axes   [{:tag "wght" :min 200.0 :max 900.0} {:tag "opsz" :min 7.0 :max 72.0}]}
          {:family "Roboto Flex"
           :fonts  {:400 {} :700 {}}
           :axes   [{:tag "GRAD" :min -200.0 :max 150.0} {:tag "wght" :min 100.0 :max 1000.0}]}
          {:family "Playfair Display SC"
           :fonts  {:400 {} :400i {} :700 {} :700i {} :900 {} :900i {}}
           :axes   []}
          {:family "Lobster"
           :fonts  {:400 {}}
           :axes   []}]})))

(def ^:private all (font/families catalogue))

(defn- family [family-name]
  (:family (font/found all family-name)))

(deftest found-test
  (testing "a family is found by its name in any case, with the axes that a blog uses"
    (is (= {:family "Literata" :axes [{:tag "opsz" :min 7.0 :max 72.0} {:tag "wght" :min 200.0 :max 900.0}]
            :weights #{200 400 900} :italic? true}
           (family " literata "))))
  (testing "a name that is no family is said, with the families named like it"
    (is (= "Google Fonts has no font named \"Literataa\". It has \"Literata\"."
           (:problem (font/found all "Literataa"))))
    (is (= "Google Fonts has no font named \"Playfair\". It has \"Playfair Display SC\"."
           (:problem (font/found all "Playfair"))))
    (is (= "Google Fonts has no font named \"Garamond\"." (:problem (font/found all "Garamond"))))))

(deftest faces-query-test
  (testing "a family with axes is asked for every value of them, in roman and italic"
    (is (= "Literata:ital,opsz,wght@0,7..72,200..900;1,7..72,200..900" (font/faces-query (family "Literata")))))
  (testing "only the axes that a blog uses, and no italic where there is none"
    (is (= "Roboto+Flex:wght@100..1000" (font/faces-query (family "Roboto Flex")))))
  (testing "a family without axes is asked for its regular and bold weights"
    (is (= "Playfair+Display+SC:ital,wght@0,400;0,700;1,400;1,700" (font/faces-query (family "Playfair Display SC"))))
    (is (= "Lobster:wght@400" (font/faces-query (family "Lobster"))))))

(def ^:private css
  "/* latin-ext */
@font-face {
  font-family: 'Literata';
  font-style: italic;
  font-weight: 200 900;
  src: url(https://fonts.gstatic.com/s/literata/v40/one.woff2) format('woff2');
  unicode-range: U+0100-02BA;
}
/* latin */
@font-face {
  font-family: 'Literata';
  font-style: normal;
  font-weight: 700;
  src: url(https://fonts.gstatic.com/s/literata/v40/two.woff2) format('woff2');
  unicode-range: U+0000-00FF;
}
")

(deftest faces-test
  (testing "each face is a file named by its family, its weight where it has one, its style and its letters"
    (is (= [{:address "https://fonts.gstatic.com/s/literata/v40/one.woff2" :file "literata-italic-latin-ext.woff2"}
            {:address "https://fonts.gstatic.com/s/literata/v40/two.woff2" :file "literata-700-normal-latin.woff2"}]
           (font/faces css "literata"))))
  (testing "the stylesheet names each face by its file beside it"
    (let [local (font/faces-css css (font/faces css "literata"))]
      (is (str/includes? local "src: url(literata-italic-latin-ext.woff2) format('woff2');"))
      (is (str/includes? local "src: url(literata-700-normal-latin.woff2) format('woff2');"))
      (is (not (str/includes? local "gstatic"))))))

(deftest licence-addresses-test
  (testing "the licence of a family is looked for under each licence of Google Fonts"
    (is (= ["https://raw.githubusercontent.com/google/fonts/main/ofl/sourceserif4/OFL.txt"
            "https://raw.githubusercontent.com/google/fonts/main/apache/sourceserif4/LICENSE.txt"
            "https://raw.githubusercontent.com/google/fonts/main/ufl/sourceserif4/UFL.txt"]
           (map :address (font/licence-addresses {:family "Source Serif 4"}))))))

(def ^:private settings-texts
  "The settings of a blog, written in each way that a map can end."
  {:new-blog    (start/settings-text "A blog")
   :one-line    "{:title \"A blog\" :per-page 5}"
   :brace-below "{:title \"A blog\"\n :per-page 5\n}"
   :empty       "{}"})

(deftest with-font-test
  (testing "the font goes on a line of its own where the settings have lines"
    (is (str/ends-with? (font/with-font (:new-blog settings-texts) "Literata")
                        ";; :link-preview \"link-preview.jpg\"\n :font \"Literata\"\n }\n"))
    (is (= "{:title \"A blog\"\n :per-page 5\n :font \"Literata\"\n}"
           (font/with-font (:brace-below settings-texts) "Literata"))))
  (testing "and after the last setting where they are one line"
    (is (= "{:title \"A blog\" :per-page 5 :font \"Literata\"}" (font/with-font (:one-line settings-texts) "Literata")))
    (is (= "{:font \"Literata\"}" (font/with-font "{}" "Literata"))))
  (testing "a font that is there is replaced"
    (is (= "{:font \"Lora\" :title \"x\"}" (font/with-font "{:font \"Literata\" :title \"x\"}" "Lora"))))
  (testing "what the author wrote stays, and reads as the settings with the font"
    (let [text (font/with-font (:new-blog settings-texts) "Literata")]
      (is (= (assoc (read-string (:new-blog settings-texts)) :font "Literata") (read-string text))))))

(deftest without-font-test
  (testing "settings that had no font are as they were once the font is set and taken away"
    (doseq [[way text] settings-texts]
      (is (= text (font/without-font (font/with-font (font/with-font text "Literata") "Lora"))) (name way))))
  (testing "the font goes from where the author put it"
    (is (= "{:title \"x\"}" (font/without-font "{:font \"A\" :title \"x\"}")))
    (is (= "{:title \"x\"}" (font/without-font "{:title \"x\" :font \"A\"}")))
    (is (= "{:title \"x\"\n :per-page 5}" (font/without-font "{:title \"x\"\n :font \"A\"\n :per-page 5}"))))
  (testing "settings with no font are left as they are"
    (is (= "{:title \"x\"}" (font/without-font "{:title \"x\"}")))))

(def ^:private run example/run-command)

(deftest list-test
  (testing "a blog with no font has that of its theme"
    (example/with-copy {}
      (fn [dir]
        (is (= {:exit 0 :said ["The blog has the font of its theme. Give it one with: bareblog font \"Literata\""]}
               (run font/list! dir nil))))))
  (testing "a font without a folder is one a reader may have installed, and one with a folder is served"
    (example/with-copy {:font "Georgia"}
      (fn [dir]
        (is (re-find #"Georgia, which a reader sees who has it installed" (first (:said (run font/list! dir nil)))))
        (fs/create-dirs (fs/path dir "fonts" "georgia"))
        (is (= ["The font of the blog is Georgia, served from fonts/georgia."] (:said (run font/list! dir nil)))))))
  (testing "a font that is no name is said to be wrong"
    (example/with-copy {:font :georgia}
      (fn [dir]
        (is (re-find #"gives :georgia as its :font" (first (:said (run font/list! dir nil)))))))))

(deftest fetch-test
  (testing "settings that cannot be read are said as they are, and nothing is fetched"
    (example/with-copy {}
      (fn [dir]
        (spit (fs/file dir "blog.edn") "{:title")
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"^blog\.edn: "
                              (run font/fetch! dir true "Literata")))
        (is (not (fs/exists? (fs/path dir "fonts"))))))))

(deftest none-test
  (testing "a font that is no name is taken away too"
    (example/with-copy {:font :literata}
      (fn [dir]
        (is (zero? (:exit (run font/none! dir true))))
        (is (not (contains? (source/read-settings dir) :font))))))
  (example/with-copy {:font "Literata"}
    (fn [dir]
      (fs/create-dirs (fs/path dir "fonts" "literata"))
      (testing "a no changes nothing"
        (is (= {:exit 1 :said ["Nothing was changed."]} (run font/none! dir false)))
        (is (= "Literata" (:font (source/read-settings dir)))))
      (testing "a yes takes the font away with its folder"
        (is (zero? (:exit (run font/none! dir true))))
        (is (nil? (:font (source/read-settings dir))))
        (is (not (fs/exists? (fs/path dir "fonts" "literata")))))
      (testing "a blog with no font has nothing to take away"
        (is (= {:exit 0 :said ["The blog has the font of its theme already."]} (run font/none! dir true)))))))
