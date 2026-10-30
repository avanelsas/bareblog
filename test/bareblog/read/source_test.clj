(ns bareblog.read.source-test
  "What is read of a blog on disk. The example blog is the blog."
  (:require [babashka.fs :as fs]
            [bareblog.app.templates :as templates]
            [bareblog.app.view :as view]
            [bareblog.blog.address :as address]
            [bareblog.example :as example]
            [bareblog.read.source :as source]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def ^:private blog (source/preview example/dir))

(defn- read-copy-as
  "What `read` gives for a copy of the example blog with the blog template `template-name`."
  [template-name read]
  (example/with-copy {:template template-name} (comp read source/preview)))

(defn- post-shown
  "What the page `told` shows of the post, for `site` as a blog with the blog template
   `template-name`."
  [site told template-name]
  (get-in (view/page "/" (assoc-in site [:blog :template] template-name) told {})
          [:lists :main :by-key "post"]))

;; The blog is read once, and the name of the blog template is then the one thing that
;; differs. This holds the view to showing a post the same way whatever the blog template. It
;; does not show that a page looks right, which is seen in a browser.
(deftest every-blog-template-shows-the-same-posts
  (let [site (source/read-site blog)]
    (testing "the example blog has posts to show"
      (is (seq (:posts site))))
    (doseq [{:keys [folder]} (:posts site)
            :let             [told  (source/read-page blog {:kind :post :folder folder})
                              shown (map (partial post-shown site told) (keys templates/by-name))]]
      (testing (str folder " is shown, with the same title, tags and blocks under every blog template")
        (is (some? (first shown)))
        (is (apply = shown)))
      (testing (str folder " names its folder, and shows its files by their full address")
        (is (= folder (get-in told [:text :folder])))
        (is (every? #(str/starts-with? % (address/post "/" folder))
                    (keep :src (tree-seq coll? seq (get-in told [:text :blocks])))))))))

(deftest home-page-test
  (let [home {:kind :home :number 1}]
    (testing "a home page that lists entries is told no post"
      (is (= {:page home} (source/read-page blog home))))
    (testing "a home page that shows the newest post is told that post, with or without the site
              as it was read"
      (let [[site alone with-site] (read-copy-as :notebook
                                                 (fn [blog]
                                                   (let [site (source/read-site blog)]
                                                     [site
                                                      (source/read-page blog home)
                                                      (source/read-page blog home site)])))]
        (is (= (:folder (first (:posts site))) (get-in alone [:text :folder])))
        (is (seq (get-in alone [:text :blocks])))
        (is (= alone with-site))))
    (testing "a blog that is served under a path names its files from there"
      (let [under (assoc blog :root "/my-blog/")
            told  (source/read-page under {:kind :post :folder "a-week-in-lisbon"})]
        (is (every? #(str/starts-with? % "/my-blog/a-week-in-lisbon/")
                    (keep :src (tree-seq coll? seq (get-in told [:text :blocks])))))))
    (testing "a tag that cannot have a page, and a folder that is no post, are what is wrong with a blog"
      (let [problems (example/with-copy {}
                       (fn [dir]
                         (fs/create-dirs (fs/path dir "posts" "Bad Name"))
                         (spit (fs/file dir "posts" "hello-bareblog" "post.md") "# Hello\n\nText.\n\n#a/b\n")
                         (:problems (source/read-site (source/preview dir)))))]
        (is (= 2 (count problems)))
        (is (re-find #"Bad Name" (first problems)))
        (is (re-find #"a/b" (second problems)))))
    (testing "a file that a text shows or links to and that is not beside it is what is wrong too"
      (example/with-copy {}
        (fn [dir]
          (spit (fs/file dir "posts" "hello-bareblog" "post.md")
                "# Hello\n\n![A photo](gone.jpg) and [notes](notes.txt), [elsewhere](https://example.com/a.pdf).\n")
          (spit (fs/file dir "posts" "hello-bareblog" "notes.txt") "notes")
          (let [missing "posts/hello-bareblog names gone.jpg, and there is no such file beside it."]
            (is (= [missing] (:problems (source/read-site (source/preview dir)))))
            (is (= [missing] (source/read-problems (assoc (source/preview dir) :read #{"hello-bareblog"}))))))))
    (testing "the files of a text as a page shows it are those it shows or links to"
      (is (= #{"harbour.svg"}
             (source/files-of blog (source/read-shown-text blog "a-week-in-lisbon")))))
    (testing "a blog that is served without its drafts has none in its list, and no page for one"
      (let [published (assoc blog :preview? false)]
        (is (some :draft? (:posts (source/read-site blog))))
        (is (not-any? :draft? (:posts (source/read-site published))))
        (is (some? (source/read-page blog {:kind :post :folder "a-new-idea"})))
        (is (nil? (source/read-page published {:kind :post :folder "a-new-idea"})))))
    (testing "a blog is published at the address in its settings, without its drafts"
      (is (= {:dir      "example"
              :root     "/my-blog/"
              :preview? false
              :settings {:address "https://avanelsas.github.io/my-blog"}}
             (source/published "example" {:address "https://avanelsas.github.io/my-blog"})))
      (is (= "/" (:root (source/published "example" {})))))
    (testing "a directory is a blog when it has settings at its top"
      (is (source/blog? "example"))
      (is (not (source/blog? "src")))
      (is (not (source/blog? nil)))
      (is (= "No blog.edn in src. A blog has one at its top." (source/refusal "src"))))
    (testing "the page of a post that is not there has no data"
      (is (nil? (source/read-page blog {:kind :post :folder "not-there"}))))))

(deftest as-published-test
  (example/with-copy {}
    (fn [dir]
      (let [blog   (source/published dir (source/read-settings dir))
            lisbon {:kind :post :folder "a-week-in-lisbon"}]
        (fs/create-dirs (fs/path dir "docs" "a-week-in-lisbon"))
        (spit (fs/file dir "docs" "site.edn")
              (pr-str {:blog {:title "As published"} :posts [{:folder "a-week-in-lisbon" :title "Published" :date "2020-01-01"}]}))
        (spit (fs/file dir "docs" "a-week-in-lisbon" "page.edn")
              (pr-str {:page lisbon :text {:folder "a-week-in-lisbon" :title "Published" :blocks []}}))
        (testing "the site as it was published last is the one in the output directory"
          (is (= "As published" (get-in (source/read-published dir) [:blog :title])))
          (is (nil? (source/read-published (str (fs/path dir "posts"))))))
        (testing "a post that is taken as published is the published one, in the list and on its page"
          (let [kept (assoc blog :as-published #{"a-week-in-lisbon"} :read #{"hello-bareblog"})]
            (is (= #{"Published" "Hello bareblog"} (set (map :title (:posts (source/read-site kept))))))
            (is (= "Published" (get-in (source/read-page kept lisbon) [:text :title])))
            (is (= "A week in Lisbon" (get-in (source/read-page blog lisbon) [:text :title])))))
        (testing "its files are the published ones too"
          (is (= (str (fs/path dir "docs" "a-week-in-lisbon"))
                 (str (source/find-media-dir (assoc blog :as-published #{"a-week-in-lisbon"}) "a-week-in-lisbon"))))
          (is (= (str (fs/path dir "posts" "a-week-in-lisbon"))
                 (str (source/find-media-dir blog "a-week-in-lisbon")))))
        (testing "only the folders that are named are read, and none when none is named"
          (is (= ["hello-bareblog"] (map :folder (:posts (source/read-site (assoc blog :read #{"hello-bareblog"}))))))
          (is (empty? (:posts (source/read-site (assoc blog :read #{}))))))
        (testing "a folder with a name that is not allowed is said, whatever is read"
          (fs/create-dirs (fs/path dir "posts" "Bad Name"))
          (is (some #(re-find #"Bad Name" %) (:problems (source/read-site (assoc blog :read #{"hello-bareblog"})))))
          (is (empty? (source/read-problems (assoc blog :read #{"hello-bareblog"})))))
        (testing "a preview shows a post with the date it was first published with"
          (spit (fs/file dir "docs" "site.edn")
                (pr-str {:blog {} :posts [{:folder "hello-bareblog" :date "2026-09-09"}
                                          {:folder "a-week-in-lisbon" :date "1999-01-01"}]}))
          (let [entry (fn [folder] (first (filter (comp #{folder} :folder)
                                                  (:posts (source/read-site (source/preview dir))))))]
            (is (= "2026-09-09" (:date (entry "hello-bareblog"))))
            (is (= "1999-01-01" (:date (entry "a-week-in-lisbon"))))
            (is (nil? (:date (entry "a-new-idea"))))
            (is (= "2026-09-09"
                   (get-in (source/read-page (source/preview dir) {:kind :post :folder "hello-bareblog"})
                           [:text :date])))))
        (testing "a text has the date that the blog gives it, and none when the blog gives none"
          (let [dated (assoc blog :dates {"hello-bareblog" "2026-10-12" "a-week-in-lisbon" "1999-01-01"})
                entry (fn [folder] (first (filter (comp #{folder} :folder) (:posts (source/read-site dated)))))]
            (is (= "2026-10-12" (:date (entry "hello-bareblog"))))
            (is (= "1999-01-01" (:date (entry "a-week-in-lisbon"))))
            (is (nil? (:date (entry "why-web-components"))))
            (is (= "2026-10-12" (get-in (source/read-page dated {:kind :post :folder "hello-bareblog"}) [:text :date])))))))))

(deftest pages-test
  (example/with-copy {}
    (fn [dir]
      (let [blog  (source/preview dir)
            about {:kind :post :folder "about"}]
        (testing "a folder among the pages is a page of the blog, and no post"
          (is (= ["about"] (map :folder (:pages (source/read-site blog)))))
          (is (not-any? #{"about"} (map :folder (:posts (source/read-site blog))))))
        (testing "its entry has its title, and says that it is not published"
          (is (= {:folder "about" :title "About" :unpublished? true}
                 (select-keys (first (:pages (source/read-site blog))) [:folder :title :date :unpublished?]))))
        (testing "its page is asked for as that of a post, and is told that it is a page of the author"
          (let [told (source/read-page blog about)]
            (is (= {:kind :standalone :folder "about"} (:page told)))
            (is (= "About" (get-in told [:text :title])))
            (is (nil? (get-in told [:text :date])))
            (is (= ["/about/boat.svg"] (keep :src (tree-seq coll? seq (get-in told [:text :blocks])))))))
        (testing "a post is still told that it is a post"
          (is (= :post (get-in (source/read-page blog {:kind :post :folder "a-week-in-lisbon"}) [:page :kind]))))
        (testing "a page is read for the readers too, and is then not marked"
          (let [published (source/published dir (source/read-settings dir))]
            (is (= [{:folder "about" :title "About"}]
                   (map #(select-keys % [:folder :title :unpublished?]) (:pages (source/read-site published)))))))
        (testing "a page that is in the published list is not marked in a preview"
          (fs/create-dirs (fs/path dir "docs"))
          (spit (fs/file dir "docs" "site.edn")
                (pr-str {:blog {} :posts [] :pages [{:folder "about" :date "2026-09-09"}]}))
          (is (= {:folder "about" :date "2026-09-09"}
                 (select-keys (first (:pages (source/read-site blog))) [:folder :date :unpublished?])))
          (fs/delete-tree (fs/path dir "docs")))
        (testing "the files beside a page are found where the page is"
          (is (= (str (fs/path dir "pages" "about")) (str (source/find-dir blog "about")))))
        (testing "a page with the name of a post is left out, and the author is told"
          (fs/create-dirs (fs/path dir "pages" "hello-bareblog"))
          (spit (fs/file dir "pages" "hello-bareblog" "page.md") "# Not shown\n")
          (let [site (source/read-site blog)]
            (is (= ["about"] (map :folder (:pages site))))
            (is (some #(re-find #"pages/hello-bareblog is left out" %) (:problems site)))
            (is (= :post (get-in (source/read-page blog {:kind :post :folder "hello-bareblog"}) [:page :kind])))))))))

(deftest link-preview-test
  (let [read-with (fn read-with
                    ([link-preview prepare!] (read-with link-preview prepare! {}))
                    ([link-preview prepare! more]
                     (example/with-copy {:link-preview link-preview}
                       (fn [dir]
                         (prepare! dir)
                         (let [site (source/read-site (merge (source/preview dir) more))]
                           {:link-picture (some-> (get-in site [:pictures :link-preview]) (dissoc :version))
                            :problems     (filter #(str/includes? % ":link-preview") (:problems site))})))))
        beside    (fn [file-name] (fn [dir] (example/write-gif! dir file-name 400 400)))
        nothing   (fn [_dir])]
    (testing "a link preview beside the settings is read as the link picture, with its size, and nothing is wrong"
      (is (= {:link-picture {:src "link-preview.gif" :width 400 :height 400} :problems []}
             (update (read-with "link-preview.gif" (beside "link-preview.gif")) :problems vec))))
    (testing "a picture that is not there is named, and the blog has none"
      (let [{:keys [link-picture problems]} (read-with "link-preview.jpg" nothing)]
        (is (nil? link-picture))
        (is (= ["blog.edn names \"link-preview.jpg\" as its :link-preview, and there is no such file beside blog.edn."]
               problems))))
    (testing "a file that is no PNG, JPEG or GIF, or not beside the settings, is named too"
      (is (re-find #"which is no name of a PNG, JPEG or GIF beside blog.edn"
                   (first (:problems (read-with "link-preview.svg" (beside "link-preview.svg"))))))
      (is (re-find #"which is no name of a PNG, JPEG or GIF beside blog.edn"
                   (first (:problems (read-with "pages/about/x.gif" nothing))))))
    (testing "a link preview taken as it was published is named where it was looked for"
      (is (= ["blog.edn names \"link-preview.gif\" as its :link-preview, and there is no such file in docs, where it was published."]
             (:problems (read-with "link-preview.gif" (beside "link-preview.gif") {:pictures-in "docs"})))))
    (testing "a blog that names no picture is told nothing of it"
      (is (= {:link-picture nil :problems []} (update (read-with nil nothing) :problems vec))))))

(deftest pictures-test
  (let [read-with (fn [settings prepare!]
                    (example/with-copy (merge {:logo nil :favicon nil} settings)
                      (fn [dir]
                        (prepare! dir)
                        (let [site (source/read-site (source/preview dir))]
                          {:pictures (update-vals (:pictures site) #(update-vals % (fn [read] (dissoc read :version))))
                           :problems (filter #(re-find #":logo|:favicon" %) (:problems site))}))))
        svg       (fn [& file-names] (fn [dir] (doseq [file-name file-names] (spit (fs/file dir file-name) "<svg/>"))))]
    (testing "one name for a logo is its picture in both modes"
      (is (= {:pictures {:logo {:light {:src "logo.svg"}}} :problems []}
             (update (read-with {:logo "logo.svg"} (svg "logo.svg")) :problems vec))))
    (testing "a logo and a favicon may have a picture for each mode"
      (is (= {:logo    {:light {:src "logo.svg"} :dark {:src "logo-dark.svg"}}
              :favicon {:light {:src "icon.svg"}}}
             (:pictures (read-with {:logo    {:light "logo.svg" :dark "logo-dark.svg"}
                                    :favicon {:light "icon.svg"}}
                                   (svg "logo.svg" "logo-dark.svg" "icon.svg"))))))
    (testing "a picture of a mode that is not there is named, and the other is read"
      (let [{:keys [pictures problems]} (read-with {:logo {:light "logo.svg" :dark "gone.svg"}} (svg "logo.svg"))]
        (is (= {:logo {:light {:src "logo.svg"}}} pictures))
        (is (= ["blog.edn names \"gone.svg\" as its :logo, and there is no such file beside blog.edn."] problems))))
    (testing "a picture of a format that its kind does not take is named, with the formats it takes"
      (is (= ["blog.edn names \"icon.webp\" as its :favicon, which is no name of an SVG, PNG or ICO beside blog.edn."]
             (:problems (read-with {:favicon "icon.webp"} (svg "icon.webp"))))))
    (testing "settings that name no picture for each mode are told so"
      (is (= ["blog.edn gives {:dark \"x.svg\"} as its :logo, which is the name of a picture, or a map with one under :light and maybe one under :dark."]
             (:problems (read-with {:logo {:dark "x.svg"}} (svg))))))
    (testing "each picture has a version that changes with what it holds"
      (example/with-copy {:logo "logo.svg"}
        (fn [dir]
          (let [version (fn [] (get-in (source/read-site (source/preview dir)) [:pictures :logo :light :version]))]
            (spit (fs/file dir "logo.svg") "<svg/>")
            (let [before (version)]
              (spit (fs/file dir "logo.svg") "<svg></svg>")
              (is (string? before))
              (is (not= before (version))))))))))

(deftest font-test
  (example/with-copy {:font "Literata"}
    (fn [dir]
      (let [blog    (source/preview dir)
            folder  (fs/path dir "fonts" "literata")
            problem "fonts/literata holds no font.css, so the font is not served. Fetch it again with: bareblog font \"Literata\""]
        (testing "a font without a folder only names itself, and is no problem"
          (is (nil? (source/find-font-dir blog)))
          (is (= ":root {\n  --font: \"Literata\";\n}\n" (source/read-font-css blog)))
          (is (not-any? #{problem} (:problems (source/read-site blog)))))
        (testing "a folder without the faces is a problem"
          (fs/create-dirs folder)
          (is (some #{problem} (:problems (source/read-site blog)))))
        (testing "a folder with its faces is served"
          (spit (fs/file folder "font.css") "@font-face { src: url(a.woff2); }")
          (spit (fs/file folder "a.woff2") "the font")
          (is (str/starts-with? (source/read-font-css blog) "@font-face { src: url(fonts/literata/a.woff2); }"))
          (is (= (str (fs/path folder "a.woff2")) (str (source/find-served-font-file blog "literata" "a.woff2"))))
          (is (nil? (source/find-served-font-file blog "lora" "a.woff2")))
          (is (nil? (source/find-served-font-file blog "literata" "b.woff2"))))
        (testing "a blog that takes its pictures from where they were published takes its font from there too"
          (let [published (fs/create-dirs (fs/path dir "docs" "fonts" "literata"))]
            (spit (fs/file published "font.css") "@font-face { src: url(published.woff2); }")
            (is (str/includes? (source/read-font-css (assoc blog :pictures-in "docs")) "fonts/literata/published.woff2"))))))))

(deftest served-picture-test
  (example/with-copy {:logo {:light "mark.svg" :dark "mark-dark.svg"}}
    (fn [dir]
      (spit (fs/file dir "mark.svg") "<svg/>")
      (let [blog (source/preview dir)]
        (testing "a picture that the settings name is served from beside them"
          (is (= (str (fs/path dir "mark.svg")) (str (source/find-served-picture blog "mark.svg")))))
        (testing "one that is not there, or that the settings do not name, is not"
          (is (nil? (source/find-served-picture blog "mark-dark.svg")))
          (is (nil? (source/find-served-picture blog "logo-dark.svg")))
          (is (nil? (source/find-served-picture blog "blog.edn"))))))))

(deftest changed-pictures-test
  (example/with-copy {}
    (fn [dir]
      (let [settings {:link-preview "link-preview.gif" :logo "mark.svg"}
            changed  #(source/find-changed-pictures dir settings)]
        (testing "a picture with nothing published of it yet is changed"
          (example/write-gif! dir "link-preview.gif" 400 400)
          (is (= ["link-preview.gif"] (changed))))
        (testing "one that is as it was published is not"
          (fs/create-dirs (fs/path dir "docs"))
          (example/write-gif! (fs/path dir "docs") "link-preview.gif" 400 400)
          (is (empty? (changed))))
        (testing "one that differs from what was published is, of every kind"
          (example/write-gif! dir "link-preview.gif" 800 400)
          (spit (fs/file dir "mark.svg") "<svg/>")
          (is (= ["link-preview.gif" "mark.svg"] (changed))))
        (testing "settings that name none, or a picture that is not there, have no changed one"
          (is (empty? (source/find-changed-pictures dir {})))
          (is (empty? (source/find-changed-pictures dir {:link-preview "gone.gif"}))))))))

;; A folder is made before its text is saved, and the rest of the blog is read meanwhile.
(deftest folder-without-text-test
  (example/with-copy {}
    (fn [dir]
      (let [blog   (source/preview dir)
            before (source/read-site blog)]
        (fs/create-dirs (fs/path dir "posts" "new-idea"))
        (fs/create-dirs (fs/path dir "drafts" "half-thought"))
        (fs/create-dirs (fs/path dir "posts" "about"))
        (let [site (source/read-site blog)]
          (testing "the rest of the blog is read as it was"
            (is (= (map :folder (:posts before)) (map :folder (:posts site)))))
          (testing "the author is told of each folder that has no text"
            (is (some #{"posts/new-idea is left out. It has no post.md."} (:problems site)))
            (is (some #{"drafts/half-thought is left out. It has no post.md."} (:problems site))))
          (testing "it is no post, and does not hide the page of its name"
            (is (not-any? #{"new-idea"} (:posts (source/list-folders blog))))
            (is (= ["about"] (map :folder (:pages site))))
            (is (= :standalone (get-in (source/read-page blog {:kind :post :folder "about"}) [:page :kind]))))
          (testing "its page is that of a folder that is not there"
            (is (nil? (source/read-page blog {:kind :post :folder "new-idea"})))))))))

(deftest kept-tags-test
  (example/with-copy {}
    (fn [dir]
      (fs/create-dirs (fs/path dir "docs"))
      (spit (fs/file dir "docs" "site.edn")
            (pr-str {:blog {} :posts [{:folder "every-kind" :title "Every kind" :tags ["Travel"]}]}))
      (let [blog (assoc (source/published dir (source/read-settings dir))
                        :read #{"a-week-in-lisbon"} :as-published #{"every-kind"})]
        (testing "a tag of a post that is read is held to the tags of the posts as they were published"
          (is (= [(str "The tags \"Travel\" in every-kind (as it was published) and \"travel\" in a-week-in-lisbon "
                       "are one tag written in different ways, and cannot each have a page. Write them one way "
                       "with: bareblog tags rename \"travel\" \"Travel\", and publish them together with: "
                       "bareblog publish --all")]
                 (source/read-problems blog))))))))

(deftest series-test
  (example/with-copy {}
    (fn [dir]
      (testing "the author sees every part of a series, a draft too"
        (is (= {"Getting to know BareBlog" ["hello-bareblog" "every-kind" "a-new-idea"]}
               (:series (source/read-site (source/preview dir))))))
      (testing "a reader sees the parts that are published, and a draft joins once it is"
        (is (= {"Getting to know BareBlog" ["hello-bareblog" "every-kind"]}
               (:series (source/read-site (source/published dir (source/read-settings dir)))))))
      (testing "a part whose folder is gone stops a publish, and is said to the author"
        (fs/move (fs/path dir "posts" "every-kind") (fs/path dir "posts" "every-block"))
        (let [problem (str "The series \"Getting to know BareBlog\" names every-kind, which is no post "
                           "or draft. Keep the series without it with: bareblog series --name \"Getting "
                           "to know BareBlog\" hello-bareblog a-new-idea. A post that was renamed goes in "
                           "its place, by its new folder or title.")]
          (is (= [problem] (source/read-problems (source/published dir (source/read-settings dir)))))
          (is (some #{problem} (:problems (source/read-site (source/preview dir)))))))
      (testing "a blog with no series has none"
        (fs/delete (fs/path dir "series.edn"))
        (is (= {} (source/read-series dir)))
        (is (nil? (:series (source/read-site (source/preview dir))))))
      (testing "a file of series that is not as the command writes it is said"
        (spit (fs/file dir "series.edn") "{\"A\" \"start\"}")
        (is (thrown-with-msg? Exception #"series.edn is not as bareblog series writes it"
                              (source/read-series dir)))))))

(deftest comments-test
  (let [lisbon {:kind :post :folder "a-week-in-lisbon"}
        for-author  (get-in (source/read-page blog lisbon) [:text :comments])
        for-readers (get-in (source/read-page (source/published example/dir {}) lisbon) [:text :comments])]
    (testing "a post is told its comments, each with its text as blocks"
      (is (= 6 (count for-author)))
      (is (every? (comp seq :blocks) for-author))
      (is (not-any? :text for-author)))
    (testing "a preview gives each comment its short id, and a page for readers gives none"
      (is (= "1f0c9a52" (:short-id (first for-author))))
      (is (= (map #(dissoc % :short-id) for-author) for-readers))
      (is (not-any? :short-id for-readers)))
    (testing "a post with no comments file is told no comments"
      (is (nil? (get-in (source/read-page blog {:kind :post :folder "hello-bareblog"}) [:text :comments])))))
  (example/with-copy {}
    (fn [dir]
      (let [blog (source/preview dir)
            file (fs/file dir "posts" "a-week-in-lisbon" "comments.edn")]
        (testing "a comments file that cannot be read is a problem, and the post shows no comments"
          (spit file "[{:id")
          (is (some #{"The comments of posts/a-week-in-lisbon cannot be read."} (:problems (source/read-site blog))))
          (is (nil? (get-in (source/read-page blog {:kind :post :folder "a-week-in-lisbon"}) [:text :comments]))))
        (testing "a page that is no post has no comments, whatever is beside it"
          (spit (fs/file dir "pages" "about" "comments.edn") "[{:id")
          (is (not-any? #(re-find #"pages/about" %) (:problems (source/read-site blog))))
          (is (nil? (get-in (source/read-page blog {:kind :post :folder "about"}) [:text :comments]))))
        (testing "a comment that is not whole is a problem"
          (spit file "[{:id \"a\"}]")
          (is (some #{"A comment of posts/a-week-in-lisbon has no id, moment, text or name."}
                    (:problems (source/read-site blog)))))))))

(deftest kept-comments-test
  (testing "comments given for a post that is taken as published, and has no published page,
            make no page"
    (let [kept (assoc (source/published example/dir {}) :as-published #{"gone"} :comments {"gone" []})]
      (is (nil? (source/read-page kept {:kind :post :folder "gone"})))
      (is (nil? (source/read-shown-text kept "gone"))))))

(deftest comment-count-test
  (let [entry (fn [site folder] (first (filter (comp #{folder} :folder) (:posts site))))]
    (testing "a post read from disk counts its comments, and one with none says nothing of them"
      (let [site (source/read-site blog)]
        (is (= 6 (:comment-count (entry site "a-week-in-lisbon"))))
        (is (not (contains? (entry site "hello-bareblog") :comment-count)))))
    (testing "a post taken as it was published counts the comments a publish of comments gives it"
      (example/with-copy {}
        (fn [dir]
          (let [blog (source/published dir (source/read-settings dir))]
            (fs/create-dirs (fs/path dir "docs"))
            (spit (fs/file dir "docs" "site.edn") (pr-str (source/read-site blog)))
            (let [kept (assoc blog :read #{} :as-published #{"a-week-in-lisbon"}
                              :comments {"a-week-in-lisbon" [{:id "x"} {:id "y"}]})]
              (is (= 2 (:comment-count (entry (source/read-site kept) "a-week-in-lisbon")))))))))))
