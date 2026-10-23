(ns bareblog.app.state-test
  (:require [bareblog.app.state :as state]
            [clojure.test :refer [deftest is testing]]))

(def site
  {:blog  {:title "Bare notes" :available-themes ["ocean" "forest"]}
   :posts [{:folder "b" :title "Second" :date "2026-02-01" :tags ["travel"]}
           {:folder "a" :title "First" :date "2025-01-01"}]})

(def notebook-site (assoc-in site [:blog :template] :notebook))

(def home {:page {:kind :home :number 1}})
(def post {:page {:kind :post :folder "a"} :text {:folder "a" :title "First" :blocks []}})

(defn- after
  "The state after `messages`, one after the other, from a page that was told nothing."
  [& messages]
  (reduce state/step (state/initial {}) messages))

(defn- local-after
  "What is local to the page after `messages`."
  [& messages]
  (:local (apply after messages)))

(deftest initial-test
  (testing "a page that was told nothing searches for nothing and has chosen nothing"
    (is (= {:root  "/"
            :local {:query "" :tag nil :date nil :preset nil :menu? false :palette? false
                    :command-key? false}}
           (state/initial {}))))
  (testing "it starts with where the top of its blog is"
    (is (= "/my-blog/" (:root (state/initial {:root "/my-blog/"})))))
  (testing "it starts with what the reader chose before, and with what the page knows of its keyboard"
    (is (= {:query "lisbon" :tag "travel" :date "2026-10" :preset "forest" :menu? false :palette? false
            :command-key? true}
           (:local (state/initial {:query "lisbon" :tag "travel" :date "2026-10" :preset "forest"
                                   :command-key? true}))))))

(deftest told-test
  (testing "what a page is told becomes its site and its page"
    (is (= {:site site :page home} (select-keys (after [:told {:site site :page home}]) [:site :page]))))
  (testing "a problem is kept, with what the page showed before"
    (let [state (after [:told {:site site :page home}] [:told {:problem "No settings."}])]
      (is (= "No settings." (:problem state)))
      (is (= site (:site state)))))
  (testing "being told the site and the page again takes the problem away"
    (is (nil? (:problem (after [:told {:problem "No settings."}] [:told {:site site :page home}])))))
  (testing "what the reader chose stays when the page is told again"
    (is (= "lisbon" (:query (local-after [:search "lisbon"] [:told {:site site :page home}]))))))

(deftest choice-test
  (testing "a search is the text that is searched for"
    (is (= "lisbon" (:query (local-after [:search "lisbon"]))))
    (is (= "" (:query (local-after [:search "lisbon"] [:search nil])))))
  (testing "a tag that is picked is chosen, and picked again it is let go"
    (is (= "travel" (:tag (local-after [:tag "travel"]))))
    (is (nil? (:tag (local-after [:tag "travel"] [:tag "travel"]))))
    (is (= "travel" (:tag (local-after [:tag-link "travel"]))))
    (is (nil? (:tag (local-after [:tag "travel"] [:tag-link "travel"]))))
    (is (= "food" (:tag (local-after [:tag "travel"] [:tag "food"])))))
  (testing "a pick of no tag changes nothing"
    (is (= "travel" (:tag (local-after [:tag "travel"] [:tag nil])))))
  (testing "clearing takes the search and the tag away, and keeps the preset"
    (is (= {:query "" :tag nil :preset "forest"}
           (select-keys (local-after [:search "x"] [:tag "travel"] [:preset "forest"] [:clear])
                        [:query :tag :preset]))))
  (testing "a pick of no preset changes nothing"
    (is (= "forest" (:preset (local-after [:preset "forest"] [:preset ""]))))
    (is (= "forest" (:preset (local-after [:preset "forest"] [:preset nil])))))
  (testing "the menu is opened and closed, by its button and by what the drawer says of itself"
    (is (true? (:menu? (local-after [:open-menu]))))
    (is (false? (:menu? (local-after [:open-menu] [:close-menu]))))
    (is (true? (:menu? (local-after [:menu true]))))
    (is (false? (:menu? (local-after [:open-menu] [:menu false])))))
  (testing "the palette is opened and closed, by its button and by what it asks for itself"
    (is (true? (:palette? (local-after [:open-palette]))))
    (is (false? (:palette? (local-after [:open-palette] [:close-palette])))))
  (testing "how far the reader has read is kept, and a page that fits keeps none"
    (is (= 0.4 (:read (local-after [:read 0.4]))))
    (is (nil? (:read (local-after [:read 0.4] [:read nil])))))
  (testing "how far the reader has read stays when the page is told its post again"
    (is (= 0.4 (:read (local-after [:told {:site site :page post}] [:read 0.4]
                                   [:told {:site site :page post}])))))
  (testing "a message with no meaning for the state changes nothing"
    (is (= (after [:search "x"]) (after [:search "x"] [:to-page 2] [:submit-search] [:unknown 1])))))

(deftest read-share-test
  (testing "how far the window has come through the page, in thousandths"
    (is (= 0.0 (state/read-share 0 800)))
    (is (= 0.5 (state/read-share 400 800)))
    (is (= 0.329 (state/read-share 263.5 800)))
    (is (= 1.0 (state/read-share 800 800))))
  (testing "on a long page the line moves within a few pixels of scrolling"
    (let [long-room 16400]
      (is (pos? (state/read-share 9 long-room)))))
  (testing "a window that scrolls past either end, as it bounces, stays within the page"
    (is (= 0.0 (state/read-share -20 800)))
    (is (= 1.0 (state/read-share 830 800))))
  (testing "a page that fits in the window has nothing to read through"
    (is (nil? (state/read-share 0 0)))
    (is (nil? (state/read-share 0 -120)))))

(deftest destination-test
  (let [on-home (after [:told {:site site :page home}] [:search "two words"])
        on-post (after [:told {:site site :page post}] [:search "two words"])]
    (testing "a page number leads to that page of the home page"
      (is (= "/page/2/" (state/destination on-home [:to-page 2])))
      (is (= "/" (state/destination on-post [:to-page 1]))))
    (testing "a search that is sent from another page leads to the home page, with the search"
      (is (= "/?q=two%20words" (state/destination on-post [:submit-search]))))
    (testing "a search that is sent from the home page leads nowhere"
      (is (nil? (state/destination on-home [:submit-search]))))
    (testing "an address starts at the top of the blog"
      (let [under (fn [& messages] (reduce state/step (state/initial {:root "/my-blog/"}) messages))]
        (is (= "/my-blog/page/2/" (state/destination (under [:told {:site site :page home}]) [:to-page 2])))
        (is (= "/my-blog/?q=x"
               (state/destination (under [:told {:site site :page post}] [:search "x"]) [:submit-search])))
        (is (= "/my-blog/a/"
               (get-in (state/shown (under [:told {:site site :page home}]))
                       [:view :lists :main :by-key "entries" :lists :entries :by-key "a" :item :href])))))
    (testing "a post that is picked leads to its address"
      (is (= "/a/" (state/destination on-home [:open-post "/a/"]))))
    (testing "any other message leads nowhere"
      (is (nil? (state/destination on-home [:search "x"])))
      (is (nil? (state/destination on-post [:tag "travel"]))))))

(deftest shown-test
  (testing "a page that was told nothing shows nothing"
    (is (= {:notice nil :view nil :asked nil :preset nil} (state/shown (state/initial {})))))
  (testing "the view knows whether the menu is open"
    (is (true? (get-in (state/shown (after [:told {:site site :page home}] [:open-menu])) [:view :item :menu?]))))
  (testing "a page that was told its site and its page shows the view of the document"
    (let [shown (state/shown (after [:told {:site site :page home}]))]
      (is (= :journal (get-in shown [:view :template])))
      (is (nil? (:notice shown)))))
  (testing "a problem is the notice, and the document is left as it is"
    (is (= {:notice "No settings." :view nil}
           (select-keys (state/shown (after [:told {:site site :page home}] [:told {:problem "No settings."}]))
                        [:notice :view]))))
  (testing "something wrong with the blog is the notice, and the page still shows"
    (let [shown (state/shown (after [:told {:site (assoc site :problems ["A is left out."]) :page home}]))]
      (is (= "A is left out." (:notice shown)))
      (is (some? (:view shown)))))
  (testing "the view is the one for what the reader chose"
    (is (= ["b"] (get-in (state/shown (after [:told {:site site :page home}] [:tag "travel"]))
                         [:view :lists :main :by-key "entries" :lists :entries :order])))
    (is (= "forest" (get-in (state/shown (after [:told {:site site :page home}] [:preset "forest"]))
                            [:view :item :preset]))))
  (testing "on the home page the address asks for what the reader chose"
    (is (= {:q "" :tag "" :date ""} (:asked (state/shown (after [:told {:site site :page home}])))))
    (is (= {:q "lisbon" :tag "travel" :date ""}
           (:asked (state/shown (after [:told {:site site :page home}] [:search "lisbon"] [:tag "travel"])))))
    (is (= {:q "" :tag "" :date "2026-10-12"}
           (:asked (state/shown (after [:told {:site site :page home}] [:month "2026-10"] [:day "2026-10-12"]))))))
  (testing "letting go of the choice lets go of the date too"
    (is (nil? (:date (local-after [:day "2026-10-12"] [:clear]))))
    (is (nil? (:date (local-after [:day ""])))))
  (testing "on any other page the address is left alone"
    (is (nil? (:asked (state/shown (after [:told {:site site :page post}] [:search "lisbon"]))))))
  (testing "the view knows whether the palette and the menu are open"
    (let [item (fn [& messages] (get-in (state/shown (apply after [:told {:site notebook-site :page post}] messages))
                                        [:view :item]))]
      (is (true? (:palette? (item [:open-palette]))))
      (is (true? (:menu? (item [:menu true]))))
      (is (false? (:menu? (item))))))
  (testing "where a choice takes effect on every page, the address asks for it on every page"
    (is (= {:q "" :tag "travel" :date ""}
           (:asked (state/shown (after [:told {:site notebook-site :page post}] [:tag "travel"]))))))
  (testing "the preset to keep is the one the reader chose"
    (is (= "forest" (:preset (state/shown (after [:preset "forest"])))))))

(deftest in-place-test
  (let [on-home (after [:told {:site site :page home}])
        on-post (after [:told {:site site :page post}])
        plain   {:button 0 :modified? false}]
    (testing "only a page of the home page shows a choice in place"
      (is (state/shows-choice? on-home))
      (is (not (state/shows-choice? on-post)))
      (is (not (state/shows-choice? (state/initial {})))))
    (testing "a blog template may show a choice on every page, once the page was told what it is"
      (is (state/shows-choice? (after [:told {:site notebook-site :page post}])))
      (is (not (state/shows-choice? (state/initial {})))))
    (testing "a plain click on the tag of an entry picks it in place on a home page of entries"
      (is (state/link-picks? on-home plain)))
    (testing "where the home page shows a post, a click on a tag of the post follows the link to the
              page of the tag, on the home page and on the page of a post"
      (is (not (state/link-picks? (after [:told {:site notebook-site :page home}]) plain)))
      (is (not (state/link-picks? (after [:told {:site notebook-site :page post}]) plain))))
    (testing "a click with a key held, or with another button, follows the link"
      (is (not (state/link-picks? on-home {:button 0 :modified? true})))
      (is (not (state/link-picks? on-home {:button 1 :modified? false}))))
    (testing "on any other page a click follows the link"
      (is (not (state/link-picks? on-post plain))))))
