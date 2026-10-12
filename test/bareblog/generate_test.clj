(ns bareblog.generate-test
  "What `generate!` leaves on disk, for a copy of the example blog. The saver is one that
   writes the address it was given, and the app is a directory of two files, so that neither a
   browser nor a build is needed."
  (:require [babashka.fs :as fs]
            [bareblog.example :as example]
            [bareblog.generate :as generate]
            [bareblog.output :as output]
            [bareblog.source :as source]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(defn- save-address [address file]
  (spit (fs/file file) address))

(defn- refuse-archive [address file]
  (if (str/includes? address "archive")
    (throw (ex-info "The page never finished." {}))
    (save-address address file)))

(defn- files
  "The files in `dir`, each as its name from there."
  [dir]
  (into (sorted-set)
        (comp (filter fs/regular-file?) (map #(str (fs/relativize dir %))))
        (fs/glob dir "**" {:hidden true})))

(defn- with-blog
  "What `check` gives for the directory of a copy of the example blog at the address
   `address`, with things in it that are not to be published, and for the directory of an app."
  [address check]
  (example/with-copy {:address address}
    (fn [dir]
      (let [app (fs/create-dirs (fs/path dir "the-app"))]
        (spit (fs/file app "index.html") "the page of the app")
        (spit (fs/file app "app.css") "the styles")
        (fs/create-dirs (fs/path app "js"))
        (spit (fs/file app "js" "app.js") "the script")
        (spit (fs/file dir "posts" "a-week-in-lisbon" "index.html") "not the page")
        (spit (fs/file dir "posts" "a-week-in-lisbon" "page.edn") "not the data")
        (spit (fs/file dir "posts" "a-week-in-lisbon" ".DS_Store") "hidden")
        (fs/create-dirs (fs/path dir "posts" "Bad Name"))
        (fs/create-dirs (fs/path dir "docs"))
        (spit (fs/file dir "docs" "stale.html") "from an earlier run")
        (check dir (str app))))))

(deftest generate-test
  (with-blog "https://avanelsas.github.io/my-blog/"
    (fn [dir app]
      (let [said    (atom [])
            pages   (generate/generate! {:save save-address :say (partial swap! said conj) :app app} dir)
            docs    (fs/path dir "docs")
            written (files docs)
            site    (source/read-site (source/published dir (source/read-settings dir)))]
        (testing "it gives the file of every page of the blog, and each is there with its data"
          (is (= (map (partial output/file-of "/my-blog/") (output/pages site)) pages))
          (is (every? written pages))
          (is (every? written (keep (partial output/data-of "/my-blog/") (output/pages site)))))
        (testing "the texts of the blog are there"
          (is (every? written (keys (output/texts "/my-blog/" site)))))
        (testing "a page was saved from its address under the top of the blog"
          (is (re-matches #"http://localhost:\d+/my-blog/archive/" (slurp (fs/file docs "archive" "index.html")))))
        (testing "the data of a page is the data of that page"
          (is (= {:kind :post :folder "a-week-in-lisbon"}
                 (:page (edn/read-string (slurp (fs/file docs "a-week-in-lisbon" "page.edn")))))))
        (testing "the files of the app are there, and the page of the app is not the home page"
          (is (= "the styles" (slurp (fs/file docs "app.css"))))
          (is (= "the script" (slurp (fs/file docs "js" "app.js"))))
          (is (str/starts-with? (slurp (fs/file docs "index.html")) "http://localhost")))
        (testing "a file beside a post is there, and none that a page could not name"
          (is (contains? written "a-week-in-lisbon/harbour.svg"))
          (is (not (contains? written "a-week-in-lisbon/.DS_Store")))
          (is (not (contains? written "a-week-in-lisbon/post.md")))
          (is (str/starts-with? (slurp (fs/file docs "a-week-in-lisbon" "index.html")) "http://localhost")))
        (testing "a draft is not there"
          (is (not-any? #(str/starts-with? % "a-new-idea/") written)))
        (testing "the author is told what is wrong with the blog, and the readers are not"
          (is (some #(re-find #"Bad Name" %) @said))
          (is (not (contains? (edn/read-string (slurp (fs/file docs "site.edn"))) :problems))))
        (testing "what was there before is gone, and nothing is left beside it"
          (is (not (contains? written "stale.html")))
          (is (not (fs/exists? (fs/path dir "docs.new"))))
          (is (not (fs/exists? (fs/path dir "docs.old")))))))))

(deftest failure-test
  (with-blog nil
    (fn [dir app]
      (let [problem (try
                      (generate/generate! {:save refuse-archive :say (constantly nil) :app app} dir)
                      nil
                      (catch Exception problem problem))]
        (testing "a page that cannot be saved stops the run, and the problem names the page"
          (is (= "Could not save archive/index.html. The page never finished." (ex-message problem))))
        (testing "what was there before is still there, and nothing is left beside it"
          (is (= #{"stale.html"} (files (fs/path dir "docs"))))
          (is (not (fs/exists? (fs/path dir "docs.new")))))))))
