(ns bareblog.app.state-test
  (:require [bareblog.app.state :as state]
            [clojure.test :refer [deftest is testing]]))

(def site
  {:blog  {:title "Bare notes" :presets ["ocean" "forest"]}
   :posts [{:folder "b" :title "Second" :date "2026-02-01" :tags ["travel"]}
           {:folder "a" :title "First" :date "2025-01-01"}]})

(def home {:page {:kind :home :number 1}})
(def post {:page {:kind :post :folder "a"} :post {:title "First" :blocks []}})

(defn- after
  "The state after `messages`, one after the other, from a page that was told nothing."
  [& messages]
  (reduce state/step (state/initial {}) messages))

(deftest initial-test
  (testing "a page that was told nothing searches for nothing and has chosen nothing"
    (is (= {:query "" :tag nil :preset nil :menu? false} (state/initial {}))))
  (testing "it starts with what the reader chose before"
    (is (= {:query "lisbon" :tag "travel" :preset "forest" :menu? false}
           (state/initial {:query "lisbon" :tag "travel" :preset "forest"})))))

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
    (is (= "lisbon" (:query (after [:search "lisbon"] [:told {:site site :page home}]))))))

(deftest choice-test
  (testing "a search is the text that is searched for"
    (is (= "lisbon" (:query (after [:search "lisbon"]))))
    (is (= "" (:query (after [:search "lisbon"] [:search nil])))))
  (testing "a tag that is picked is chosen, and picked again it is let go"
    (is (= "travel" (:tag (after [:tag "travel"]))))
    (is (nil? (:tag (after [:tag "travel"] [:tag "travel"]))))
    (is (= "food" (:tag (after [:tag "travel"] [:tag "food"])))))
  (testing "a pick of no tag changes nothing"
    (is (= "travel" (:tag (after [:tag "travel"] [:tag nil])))))
  (testing "clearing takes the search and the tag away, and keeps the preset"
    (is (= {:query "" :tag nil :preset "forest" :menu? false}
           (after [:search "x"] [:tag "travel"] [:preset "forest"] [:clear]))))
  (testing "a pick of no preset changes nothing"
    (is (= "forest" (:preset (after [:preset "forest"] [:preset ""]))))
    (is (= "forest" (:preset (after [:preset "forest"] [:preset nil])))))
  (testing "the menu is opened and closed, by its button and by what the drawer says of itself"
    (is (true? (:menu? (after [:open-menu]))))
    (is (false? (:menu? (after [:open-menu] [:close-menu]))))
    (is (true? (:menu? (after [:menu true]))))
    (is (false? (:menu? (after [:open-menu] [:menu false])))))
  (testing "a message with no meaning for the state changes nothing"
    (is (= (after [:search "x"]) (after [:search "x"] [:to-page 2] [:submit-search] [:unknown 1])))))

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
    (is (= {:q "" :tag ""} (:asked (state/shown (after [:told {:site site :page home}])))))
    (is (= {:q "lisbon" :tag "travel"}
           (:asked (state/shown (after [:told {:site site :page home}] [:search "lisbon"] [:tag "travel"]))))))
  (testing "on any other page the address is left alone"
    (is (nil? (:asked (state/shown (after [:told {:site site :page post}] [:search "lisbon"]))))))
  (testing "the preset to keep is the one the reader chose"
    (is (= "forest" (:preset (state/shown (after [:preset "forest"])))))))

(deftest in-place-test
  (let [on-home (after [:told {:site site :page home}])
        on-post (after [:told {:site site :page post}])
        plain   {:button 0 :modified? false}]
    (testing "only a page of the home page shows a choice in place"
      (is (state/chooses-in-place? on-home))
      (is (not (state/chooses-in-place? on-post)))
      (is (not (state/chooses-in-place? (state/initial {})))))
    (testing "a plain click on a tag picks it in place on the home page"
      (is (state/picks-in-place? on-home plain)))
    (testing "a click with a key held, or with another button, follows the link"
      (is (not (state/picks-in-place? on-home {:button 0 :modified? true})))
      (is (not (state/picks-in-place? on-home {:button 1 :modified? false}))))
    (testing "on any other page a click follows the link"
      (is (not (state/picks-in-place? on-post plain))))))
