(ns bareblog.app.card-test
  (:require [bareblog.app.card :as card]
            [clojure.test :refer [deftest is testing]]))

(def blog {:title "Bare notes" :address "https://example.org/notes/"})

(def site {:blog blog :root "/notes/"})

(def post
  {:title   "A week in Lisbon"
   :summary "Tiles, trams and fish."
   :blocks  [{:kind :text :text "Tiles."}
             {:kind :image :src "/notes/a-week/map.svg" :alt "A map"}
             {:kind :image :src "/notes/a-week/tram.png" :alt "A tram" :width 1200 :height 800}
             {:kind :image :src "/notes/a-week/fish.jpg" :alt "Fish"}]})

(def a-week {:kind :post :folder "a-week"})

(def square {:src "link-preview.jpg" :width 400 :height 400})

(defn- card-of
  "The card of `page` showing `text`, of `site`."
  ([page text] (card-of site page text))
  ([site page text] (card/card {:site site :page page :text text})))

(deftest post-test
  (testing "a post is an article, with its own title and summary, its full address, and its
            first picture that is no SVG, with its size"
    (is (= {:title       "A week in Lisbon"
            :kind        :article
            :site-name   "Bare notes"
            :url         "https://example.org/notes/a-week/"
            :description "Tiles, trams and fish."
            :picture     {:at "https://example.org/notes/a-week/tram.png" :width 1200 :height 800}}
           (card-of a-week post))))
  (testing "a page of the author is an article too"
    (is (= :article (:kind (card-of {:kind :standalone :folder "about"} {:title "About"})))))
  (testing "a post whose pictures are all SVG or WebP, and that has no summary, has neither"
    (let [card (card-of a-week {:title  "Maps"
                                :blocks [{:kind :image :src "/notes/a-week/map.svg"}
                                         {:kind :image :src "/notes/a-week/map.webp"}]})]
      (is (not-any? card [:picture :description]))))
  (testing "a picture outside the blog is passed over for the next one"
    (is (= "https://example.org/notes/a-week/tram.png"
           (get-in (card-of a-week (assoc post :blocks [{:kind :image :src "/elsewhere/map.png"}
                                                        {:kind :image :src "/notes/a-week/tram.png"}]))
                   [:picture :at])))))

(deftest addresses-test
  (testing "in the preview, the top of the blog is /, and the addresses are still those of the blog"
    (let [card (card-of (assoc site :root "/") a-week (assoc post :blocks [{:kind :image :src "/a-week/tram.png"}]))]
      (is (= "https://example.org/notes/a-week/" (:url card)))
      (is (= {:at "https://example.org/notes/a-week/tram.png"} (:picture card)))))
  (testing "a picture at a full address keeps it"
    (is (= "https://cdn.example.com/a.gif"
           (get-in (card-of a-week (assoc post :blocks [{:kind :image :src "https://cdn.example.com/a.gif"}]))
                   [:picture :at]))))
  (testing "a blog whose settings name no address has no address in its card, and no picture"
    (is (= {:title "A week in Lisbon" :kind :article :site-name "Bare notes" :description "Tiles, trams and fish."}
           (card-of {:blog (dissoc blog :address) :root "/" :link-picture square} a-week post)))))

(deftest link-picture-test
  (let [with-square (assoc site :link-picture square)
        bare        (assoc post :blocks [{:kind :text :text "Tiles."}])]
    (testing "a post with no picture of its own has that of the blog, from its top, with its size"
      (is (= {:at "https://example.org/notes/link-preview.jpg" :width 400 :height 400}
             (:picture (card-of with-square a-week bare)))))
    (testing "the home page, a tag and the archive have it too, and are the blog, by its name"
      (doseq [page [{:kind :home :number 1} {:kind :tag :tag "travel"} {:kind :archive}]]
        (is (= {:title "Bare notes" :kind :website}
               (select-keys (card-of with-square page nil) [:title :kind])))
        (is (= "https://example.org/notes/link-preview.jpg" (get-in (card-of with-square page nil) [:picture :at])))))
    (testing "a post with a picture of its own keeps it"
      (is (= "https://example.org/notes/a-week/tram.png"
             (get-in (card-of with-square a-week post) [:picture :at]))))))

(deftest other-pages-test
  (testing "the address of the home page, of a tag and of the archive"
    (is (= ["https://example.org/notes/" "https://example.org/notes/tags/travel/" "https://example.org/notes/archive/"]
           (map #(:url (card-of % nil)) [{:kind :home :number 1} {:kind :tag :tag "travel"} {:kind :archive}])))))

(defn- spelled
  "The tags of `card`, each as its attribute, its name and its content."
  [card]
  (map (juxt :attribute :name :content) (card/tags card)))

(deftest tags-test
  (let [card {:title "A week" :kind :article :site-name "Bare notes" :url "https://example.org/notes/a-week/"
              :description "Tiles." :picture {:at "https://example.org/notes/p.png" :width 1200 :height 630}}]
    (testing "a card is spelled out as the tags of Open Graph, in order, each named by a property,
              and the card of Twitter by a name, with a wide picture beneath the text"
      (is (= [["property" "og:title" "A week"] ["property" "og:type" "article"]
              ["property" "og:site_name" "Bare notes"] ["property" "og:url" "https://example.org/notes/a-week/"]
              ["property" "og:description" "Tiles."] ["property" "og:image" "https://example.org/notes/p.png"]
              ["property" "og:image:width" "1200"] ["property" "og:image:height" "630"]
              ["name" "twitter:card" "summary_large_image"]]
             (spelled card))))
    (testing "a square picture is shown small beside the text, and so is a card with none"
      (is (= ["name" "twitter:card" "summary"] (last (spelled (assoc card :picture square)))))
      (is (= ["name" "twitter:card" "summary"] (last (spelled (dissoc card :picture))))))
    (testing "a picture of unknown size is taken to be wide, and has no size"
      (let [tags (spelled (assoc card :picture {:at "https://example.org/notes/p.png"}))]
        (is (= ["name" "twitter:card" "summary_large_image"] (last tags)))
        (is (not-any? #{"og:image:width"} (map second tags)))))))

(deftest changes-test
  (let [title  {:attribute "property" :name "og:title" :content "A week"}
        wanted [title {:attribute "property" :name "og:type" :content "article"}]]
    (testing "a head with no card gets every tag"
      (is (= {:gone [] :written wanted} (card/changes {} wanted))))
    (testing "a tag with new content is written, one as it is is not, and one not wanted goes"
      (is (= {:gone ["og:image"] :written [title]}
             (card/changes {"og:title" "Old" "og:type" "article" "og:image" "p.png"} wanted))))
    (testing "a card with no tags takes every tag away"
      (is (= {:gone ["og:title"] :written []} (card/changes {"og:title" "A week"} nil))))))
