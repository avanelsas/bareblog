(ns bareblog.app.blocks-test
  (:require [bareblog.app.blocks :as blocks]
            [bareblog.app.live :as live]
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

(def text
  {:kind    :text
   :content ["We took the " {:kind :strong :content ["tram 28"]} " to "
             {:kind :link :href "https://example.com" :title "A hill" :content ["Alfama"]}
             ", " {:kind :emphasis :content ["slowly"]} ", with " {:kind :code :content ["bb"]}
             " and " {:kind :image :src "a.png" :alt "a map"} ". "
             {:kind :strikethrough :content ["Not this."]} {:kind :break}
             "See the note" {:kind :footnote-mark :number 1 :note [{:kind :text :content ["The note."]}]} " twice"
             {:kind :footnote-mark :number 1} ". " {:kind :unknown :type :formula :text "x + y"}]})

(def examples
  "One block of each kind."
  {:text      text
   :plain     {:kind :plain :content ["plain " {:kind :emphasis :content ["text"]}]}
   :heading   {:kind :heading :level 2 :content ["The " {:kind :emphasis :content ["old"]} " town"]}
   :photo     {:kind :photo :src "harbour.svg" :alt "The harbour at dawn"}
   :quote     {:kind :quote :blocks [{:kind :text :content ["One."]} {:kind :text :content ["Two."]}]}
   :list      {:kind  :list
               :items [{:blocks [{:kind :plain :content ["one"]}
                                 {:kind :list :items [{:blocks [{:kind :plain :content ["below"]}]}]}]}
                       {:blocks [{:kind :text :content ["two"]}]}]}
   :code      {:kind :code :language "js" :text "const a = 1;\n"}
   :live      {:kind :live :text "<x-icon name=\"star\"></x-icon>\n"}
   :divider   {:kind :divider}
   :table     {:kind :table
               :head [{:content ["Name"] :align :left} {:content ["Size"] :align :right}]
               :rows [[{:content ["a"] :align :left} {:content ["1"] :align :right}]
                      [{:content ["b"]} {:content ["2"] :align :center}]]}
   :footnotes {:kind  :footnotes
               :notes [{:number 1 :blocks [{:kind :text :content ["The note."]}]}]}
   :unknown   {:kind :unknown :type :todo-list :text "A list of things to do."}})

(deftest template-test
  (testing "a text block is a paragraph, and each thing it holds is an element of its kind"
    (is (= [:p "We took the " [:strong "tram 28"] " to "
            [:a {:href "https://example.com" :title "A hill"} "Alfama"]
            ", " [:em "slowly"] ", with " [:code "bb"]
            " and " [:img {:src "a.png" :alt "a map"}] ". "
            [:s "Not this."] [:br]
            "See the note" [:span {:class "note-mark"}
                            [:sup [:a {:id "note-mark-1" :href "#note-1"} "1"]]
                            [:span {:class "sidenote" :role "note"} [:span {:class "sidenote-number"} "1"] "The note."]]
            " twice"
            [:sup [:a {:href "#note-1"} "1"]] ". " "x + y"]
           (blocks/template (:text examples)))))
  (testing "a link with no title has no title attribute"
    (is (= [:p [:a {:href "/x"} "x"]]
           (blocks/template {:kind :text :content [{:kind :link :href "/x" :content ["x"]}]}))))
  (testing "a heading block is a heading of its level"
    (is (= [:h2 "The " [:em "old"] " town"] (blocks/template (:heading examples)))))
  (testing "a photo block is an x-image in a figure"
    (is (= [:figure [:x-image {:src "harbour.svg" :alt "The harbour at dawn"}]]
           (blocks/template (:photo examples)))))
  (testing "a photo with a size holds its place with a ratio"
    (is (= [:x-image {:src "harbour.svg" :alt "The harbour at dawn" :ratio "600:450"}]
           (peek (blocks/template (assoc (:photo examples) :width 600 :height 450))))))
  (testing "a photo with a size has it on its figure"
    (is (= [:figure {:style "--photo-width: 600px; --photo-ratio: 600 / 450"}]
           (pop (blocks/template (assoc (:photo examples) :width 600 :height 450))))))
  (testing "a vector has no width of its own, only its shape"
    (is (= [:figure {:style "--photo-ratio: 30 / 40"}]
           (pop (blocks/template (assoc (:photo examples) :width 30 :height 40 :vector? true))))))
  (testing "a photo of 3 by 2 or wider is a landscape"
    (is (= "landscape"
           (:class (second (blocks/template (assoc (:photo examples) :width 600 :height 400))))))
    (is (not (blocks/landscape? {:width 599 :height 400})))
    (is (not (blocks/landscape? {}))))
  (testing "an image with a size has its width and height"
    (is (= [:p [:img {:src "a.png" :alt "a map" :width 64 :height 32}]]
           (blocks/template {:kind :text :content [{:kind :image :src "a.png" :alt "a map" :width 64 :height 32}]}))))
  (testing "an unknown block is a paragraph with its plain text"
    (is (= [:p "A list of things to do."] (blocks/template (:unknown examples))))))

(deftest quote-test
  (is (= [:blockquote [:p "One."] [:p "Two."]] (blocks/template (:quote examples)))))

(deftest list-test
  (testing "an item holds the template of each of its blocks, and plain text is a span"
    (is (= [:ul [:li [:span "one"] [:ul [:li [:span "below"]]]] [:li [:p "two"]]]
           (blocks/template (:list examples)))))
  (testing "a numbered list that starts at one says nothing of its start"
    (is (= [:ol [:li [:span "one"]]]
           (blocks/template {:kind :list :numbered? true :start 1
                             :items [{:blocks [{:kind :plain :content ["one"]}]}]}))))
  (testing "a numbered list that starts elsewhere says where"
    (is (= [:ol {:start 3} [:li [:span "three"]]]
           (blocks/template {:kind :list :numbered? true :start 3
                             :items [{:blocks [{:kind :plain :content ["three"]}]}]})))))

(deftest code-test
  (testing "code is an x-code with its language and a button to copy it"
    (is (= [:x-code {:language "js" :show-copy true} "const a = 1;\n"]
           (blocks/template (:code examples)))))
  (testing "code with no language names none"
    (is (= [:x-code {:show-copy true} "plain"] (blocks/template {:kind :code :text "plain"})))))

(deftest live-test
  (let [code "<x-icon name=\"star\"></x-icon>\n"]
    (testing "a live block is a panel of a frame that shows the document of its code, above the
              code"
      (is (= [:div {:class "live"}
              [:iframe {:class   "live-frame"
                        :title   "A live example"
                        :sandbox "allow-scripts"
                        :srcdoc  (live/document code nil)}]
              [:x-code {:language "html" :show-copy true} code]]
             (blocks/template (:live examples)))))
    (testing "a live block with a height gives its frame that height, and a document that keeps it"
      (is (= [:div {:class "live"}
              [:iframe {:class   "live-frame"
                        :title   "A live example"
                        :sandbox "allow-scripts"
                        :style   "height:300px"
                        :srcdoc  (live/document code 300)}]
              [:x-code {:language "html" :show-copy true} code]]
             (blocks/template (assoc (:live examples) :height 300)))))))

(deftest divider-test
  (is (= [:hr] (blocks/template (:divider examples)))))

(deftest table-test
  (testing "a table is an x-table with a column for each cell of its head, a row of headers,
            and a row for each of its rows"
    (is (= [:x-table {:columns "repeat(2, auto)" :bordered true :striped true}
            [:x-table-row
             [:x-table-cell {:type "header" :scope "col" :align "start"} "Name"]
             [:x-table-cell {:type "header" :scope "col" :align "end"} "Size"]]
            [:x-table-row [:x-table-cell {:align "start"} "a"] [:x-table-cell {:align "end"} "1"]]
            [:x-table-row [:x-table-cell "b"] [:x-table-cell {:align "center"} "2"]]]
           (blocks/template (:table examples))))))

(deftest footnotes-test
  (testing "the notes are a numbered list, and each note has its blocks and a way back to its mark"
    (is (= [:section {:class "footnotes"}
            [:ol [:li {:id "note-1"}
                  [:p "The note."]
                  [:a {:class "note-back" :href "#note-mark-1" :aria-label "Back to the text"} "\u21a9"]]]]
           (blocks/template (:footnotes examples)))))
  (testing "the first mark of a note holds the note beside it, of the text of its blocks"
    (is (= [:p "See" [:span {:class "note-mark"}
                      [:sup [:a {:id "note-mark-1" :href "#note-1"} "1"]]
                      [:span {:class "sidenote" :role "note"} [:span {:class "sidenote-number"} "1"]
                       "One " [:em "two"] "." " " "Three."]]]
           (blocks/template {:kind    :text
                             :content ["See" {:kind   :footnote-mark :number 1
                                              :note   [{:kind :text :content ["One " {:kind :emphasis :content ["two"]} "."]}
                                                       {:kind :code :text "x"}
                                                       {:kind :text :content ["Three."]}]}]})))))

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
    (is (= {:kind :marquee}
           (ex-data (try (blocks/template {:kind :text :content [{:kind :marquee}]})
                         (catch #?(:clj Exception :cljs :default) e e)))))))
