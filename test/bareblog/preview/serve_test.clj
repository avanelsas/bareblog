(ns bareblog.preview.serve-test
  (:require [babashka.fs :as fs]
            [bareblog.blog.address :as address]
            [bareblog.blog.text :as text]
            [bareblog.example :as example]
            [bareblog.preview.serve :as serve]
            [bareblog.read.source :as source]
            [bareblog.written.output :as output]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(deftest page-routes-test
  (testing "the top of the blog is the first page of the home page"
    (is (= {:serve :page :page {:kind :home :number 1}} (serve/route "/"))))
  (testing "a later page of the home page has its number"
    (is (= {:serve :page :page {:kind :home :number 2}} (serve/route "/page/2/")))
    (is (= {:serve :nothing} (serve/route "/page/0/")))
    (is (= {:serve :nothing} (serve/route "/page/two/"))))
  (testing "the page of a tag has the tag as it is written, not as its address spells it"
    (is (= {:serve :page :page {:kind :tag :tag "travel"}} (serve/route "/tags/travel/")))
    (is (= {:serve :page :page {:kind :tag :tag "web components"}} (serve/route "/tags/web%20components/"))))
  (testing "a slash that is spelled as an escape is part of the tag"
    (is (= {:serve :page :page {:kind :tag :tag "a/b"}} (serve/route "/tags/a%2Fb/")))
    (is (= {:serve :nothing} (serve/route "/tags/a/b/"))))
  (testing "the server has the page of every tag that the app links to"
    (doseq [tag-name ["travel" "web components" "a/b" "c++" "100%" "café" "what?"]]
      (is (= {:serve :page :page {:kind :tag :tag tag-name}} (serve/route (address/tag "/" tag-name))))))
  (testing "a tag with a broken escape serves nothing"
    (is (= {:serve :nothing} (serve/route "/tags/%zz/"))))
  (testing "the archive"
    (is (= {:serve :page :page {:kind :archive}} (serve/route "/archive/"))))
  (testing "the address of a post serves its page"
    (is (= {:serve :page :page {:kind :post :folder "a-week-in-lisbon"}} (serve/route "/a-week-in-lisbon/"))))
  (testing "the data and the socket of a page are beside the page, for every kind of page"
    (is (= {:serve :data :page {:kind :home :number 1}} (serve/route "/page.edn")))
    (is (= {:serve :changes :page {:kind :home :number 2}} (serve/route "/page/2/changes")))
    (is (= {:serve :data :page {:kind :tag :tag "travel"}} (serve/route "/tags/travel/page.edn")))
    (is (= {:serve :changes :page {:kind :archive}} (serve/route "/archive/changes")))
    (is (= {:serve :data :page {:kind :post :folder "a-post"}} (serve/route "/a-post/page.edn")))
    (is (= {:serve :changes :page {:kind :post :folder "a-post"}} (serve/route "/a-post/changes"))))
  (testing "the address of a page without its last slash redirects to it"
    (is (= {:serve :redirect :to "/a-week-in-lisbon/"} (serve/route "/a-week-in-lisbon")))
    (is (= {:serve :redirect :to "/archive/"} (serve/route "/archive")))
    (is (= {:serve :redirect :to "/page/2/"} (serve/route "/page/2")))))

(deftest file-routes-test
  (testing "the data of the site is at the top"
    (is (= {:serve :site} (serve/route "/site.edn"))))
  (testing "any other file beside a post is the media of that post"
    (is (= {:serve :media :folder "a-week-in-lisbon" :file "harbour.svg"}
           (serve/route "/a-week-in-lisbon/harbour.svg"))))
  (testing "a percent escape in the name of a file is read as its character"
    (is (= {:serve :media :folder "a-post" :file "my photo.jpg"} (serve/route "/a-post/my%20photo.jpg")))
    (is (= {:serve :media :folder "a-post" :file "café.jpg"} (serve/route "/a-post/caf%C3%A9.jpg"))))
  (testing "the server sends every file that a post can name beside it"
    (doseq [source ["harbour.jpg" "my%20photo.jpg" "caf%C3%A9.jpg" "a+b.jpg" "photo.final.JPG"]]
      (is (= {:serve :media :folder "a-post" :file (text/file-beside source)}
             (serve/route (str "/a-post/" source))))))
  (testing "a file at the top, and anything under js, is the app"
    (is (= {:serve :static :file "journal.css"} (serve/route "/journal.css")))
    (is (= {:serve :static :file "js/cljs-runtime/cljs.core.js"}
           (serve/route "/js/cljs-runtime/cljs.core.js")))))

(deftest nothing-test
  (testing "a path that names nothing serves nothing"
    (is (= {:serve :nothing} (serve/route "/a/b/c/d")))
    (is (= {:serve :nothing} (serve/route "/Not-A-Folder/"))))
  (testing "a hidden file and a broken escape serve nothing"
    (is (= {:serve :nothing} (serve/route "/a-post/.env")))
    (is (= {:serve :nothing} (serve/route "/a-post/a%zz.jpg"))))
  (testing "a path with two dots in a row serves nothing"
    (is (= {:serve :nothing} (serve/route "/js/../../blog.edn")))
    (is (= {:serve :nothing} (serve/route "/a-post/..")))
    (is (= {:serve :nothing} (serve/route "/a-post/%2e%2e")))))

(deftest top-files-test
  (testing "the feed, and the page for an address with no page, are at the top"
    (is (= {:serve :feed} (serve/route "/feed.xml")))
    (is (= {:serve :missing} (serve/route "/404.html")))))

(deftest under-a-path-test
  (testing "a path of a request is read from the top of the blog"
    (is (= "/archive/" (serve/below "/my-blog/" "/my-blog/archive/")))
    (is (= "/" (serve/below "/my-blog/" "/my-blog/")))
    (is (= "/archive/" (serve/below "/" "/archive/"))))
  (testing "a path that is not under the top of the blog is none"
    (is (nil? (serve/below "/my-blog/" "/archive/")))
    (is (nil? (serve/below "/my-blog/" "/my-blog"))))
  (testing "a path from the top of the blog is written as a path of a request"
    (is (= "/my-blog/archive/" (serve/above "/my-blog/" "/archive/")))
    (is (= "/archive/" (serve/above "/" "/archive/")))))

(deftest document-test
  (let [html "<html lang=\"en\" data-loading>\n<link href=\"/app.css\">\n<script src=\"/js/app.js\"></script>"]
    (testing "the page of the app names its files from the top of the blog, and says where that is"
      (is (= "<html data-root=\"/my-blog/\" lang=\"en\" data-loading>\n<link href=\"/my-blog/app.css\">\n<script src=\"/my-blog/js/app.js\"></script>"
             (serve/document html {:root "/my-blog/"}))))
    (testing "at the top of a host it is as it was, but for where the top is"
      (is (= (str/replace html "<html " "<html data-root=\"/\" ") (serve/document html {:root "/"}))))
    (testing "a file of the app is named with its version, and what has none is named as it was"
      (is (= (str "<html data-root=\"/my-blog/\" lang=\"en\" data-loading>\n<link href=\"/my-blog/app.css\">\n"
                  "<script src=\"/my-blog/js/app.js?v=abc\"></script>")
             (serve/document html {:root "/my-blog/" :versions {"js/app.js" "abc" "blog.css" "def"}}))))
    (testing "the page for an address with no page says that it is that"
      (is (str/starts-with? (serve/document html {:root "/" :missing? true}) "<html data-root=\"/\" data-not-found lang")))
    (testing "the page of the app itself is one that this can be done to"
      (let [page (serve/document (slurp "resources/public/index.html") {:root "/my-blog/"})]
        (is (str/includes? page "data-root=\"/my-blog/\""))
        (is (str/includes? page "src=\"/my-blog/js/app.js\""))
        (is (str/includes? page "href=\"/my-blog/feed.xml\""))
        (is (not (re-find #"(href|src)=\"/(?!my-blog/)" page)))))))

(deftest every-page-is-served-test
  (let [site    {:blog  {:per-page 1}
                 :posts [{:folder "b" :tags ["travel" "web components" "c++" "café" "100%"]} {:folder "a"}]
                 :pages [{:folder "about"}]}
        ;; The address of a page of the author is that of a post, so it is asked for as one.
        asked-as (fn [page] (cond-> page (= :standalone (:kind page)) (assoc :kind :post)))]
    (testing "the server has, under the top of a blog, every page that a generated blog holds"
      (doseq [root ["/" "/my-blog/"]
              page (remove (comp #{:not-found} :kind) (output/pages site))]
        (is (= {:serve :page :page (asked-as page)} (serve/route (serve/below root (address/of-page root page))))
            (str root " " page))))
    (testing "and the page for an address with no page"
      (is (= {:serve :missing} (serve/route (serve/below "/my-blog/" (address/of-page "/my-blog/" {:kind :not-found}))))))))

(defn- fetched
  "What `blog` answers to a request for `path`, read as data."
  [blog path]
  (edn/read-string (:body (serve/handler {:blog blog} {:uri path}))))

(deftest site-data-test
  (example/with-copy {:address "https://avanelsas.github.io/my-blog/"}
    (fn [dir]
      (spit (str dir "/posts/hello-bareblog/post.md") "# Hello\n\nText.\n\n#a/b\n")
      (let [settings (source/read-settings dir)]
        (testing "the author is told what is wrong with the blog"
          (is (seq (:problems (fetched (source/preview dir) "/site.edn")))))
        (testing "the readers are not, and the blog is under the path of its address"
          (is (not (contains? (fetched (source/published dir settings) "/my-blog/site.edn") :problems)))
          (is (= 404 (:status (serve/handler {:blog (source/published dir settings) :app (serve/app-dir ".")}
                                                {:uri "/site.edn"})))))
        (testing "the data of a page names a socket for the author, and none for the readers"
          (is (= "changes" (:changes (fetched (source/preview dir) "/archive/page.edn"))))
          (is (= {:page {:kind :archive}}
                 (fetched (source/published dir settings) "/my-blog/archive/page.edn"))))))))

;; The files of the app are in a directory that the server is told of, which is not the one
;; that a command is run in.
(deftest app-test
  (example/with-copy {}
    (fn [dir]
      (let [app    (fs/create-dirs (fs/path dir "the-app" "js"))
            served (fn [blog path] (serve/handler {:blog blog :app (fs/parent app)} {:uri path}))
            under  (assoc (source/preview dir) :root "/my-blog/")]
        (spit (fs/file (fs/parent app) "index.html") "<html lang=\"en\"><link href=\"/app.css\">")
        (spit (fs/file (fs/parent app) "app.css") "the styles")
        (spit (fs/file app "app.js") "the script")
        (testing "a file of the app is sent from that directory"
          (is (= "the styles" (slurp (:body (served (source/preview dir) "/app.css")))))
          (is (= "the script" (slurp (:body (served under "/my-blog/js/app.js"))))))
        (testing "the page of the app is the one in that directory, for the top of the blog, and
                  names each file with a version that is the same until the file changes"
          (let [page    (:body (served under "/my-blog/archive/"))
                version (second (re-find #"app\.css\?v=([0-9a-f]{12})\"" page))]
            (is (some? version))
            (is (= (str "<html data-root=\"/my-blog/\" lang=\"en\"><link href=\"/my-blog/app.css?v=" version "\">")
                   page))
            (is (= page (:body (served under "/my-blog/archive/"))))
            (spit (fs/file (fs/parent app) "app.css") "other styles")
            (is (not= page (:body (served under "/my-blog/archive/"))))
            (spit (fs/file (fs/parent app) "app.css") "the styles")))
        (testing "a file in a folder of the app has a version too, under its name from the top"
          (spit (fs/file (fs/parent app) "index.html") "<html lang=\"en\"><script src=\"/js/app.js\">")
          (is (re-find #"src=\"/my-blog/js/app\.js\?v=[0-9a-f]{12}\"" (:body (served under "/my-blog/archive/"))))
          (spit (fs/file (fs/parent app) "index.html") "<html lang=\"en\"><link href=\"/app.css\">"))
        (testing "a file is sent whatever version it is asked for with"
          (is (= "the styles"
                 (slurp (:body (serve/handler {:blog under :app (fs/parent app)}
                                              {:uri "/my-blog/app.css" :query-string "v=abc"}))))))
        (testing "a file that is not in it is not found"
          (is (= 404 (:status (served (source/preview dir) "/other.css")))))))))

(deftest app-dir-test
  (testing "the files of the app are in BareBlog, wherever that is"
    (is (= "/somewhere/bareblog/resources/public" (str (serve/app-dir "/somewhere/bareblog"))))
    (is (fs/regular-file? (fs/path (serve/app-dir ".") "index.html")))))

;; The preview sends of the files beside a text what a publish copies of them.
(deftest media-test
  (example/with-copy {}
    (fn [dir]
      (let [blog   (source/preview dir)
            served (fn [path] (serve/handler {:blog blog :app "resources/public"} {:uri path}))]
        (spit (fs/file dir "posts" "a-week-in-lisbon" "post.md~") "what an editor left")
        (spit (fs/file dir "posts" "a-week-in-lisbon" "unused.svg") "<svg/>")
        (testing "a file that the text shows is sent"
          (is (= 200 (:status (served "/a-week-in-lisbon/harbour.svg")))))
        (testing "a file beside the text that it does not name is not found, as when it is published"
          (is (= 404 (:status (served "/a-week-in-lisbon/unused.svg"))))
          (is (= 404 (:status (served "/a-week-in-lisbon/post.md~")))))
        (testing "a file of a folder that is not there is not found"
          (is (= 404 (:status (served "/no-such-post/harbour.svg")))))))))
