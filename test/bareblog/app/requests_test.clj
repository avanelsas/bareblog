(ns bareblog.app.requests-test
  (:require [bareblog.app.templates :as templates]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]))

(defn- requested-values-def? [form]
  (and (seq? form) (= '(def requested-values) (take 2 form))))

;; `baredom.requests` is ClojureScript only, so the tests read it as data.
(defn- read-requested-values
  "What each request event of BareDOM asks of each attribute, by the name of the event."
  []
  (->> (str "[" (slurp (io/resource "baredom/requests.cljs")) "]")
       edn/read-string
       (filter requested-values-def?)
       first
       last))

(defn- part-attributes
  "The part name of `hole`, with the attributes it has a hole for, as text."
  [{:keys [part attrs]}]
  [part (set (map name (keys attrs)))])

(defn- hole-attributes
  "The attributes that `split` has a hole for, by part name, as text."
  [split]
  (into {} (map part-attributes) (:holes split)))

(defn- unanswered
  "The request events of `split` that ask for an attribute their part has no hole for, each as
   the event, the part and the attribute."
  [requested split]
  (let [holes (hole-attributes split)]
    (for [[[event-type part] _] (:events split)
          attribute             (keys (requested event-type))
          :when                 (not (contains? (holes part) attribute))]
      [event-type part attribute])))

;; `listen!` cancels a request when its part does not show the asked value afterwards. A part
;; with no hole for that attribute shows it never, so it refuses every request.
(deftest every-request-has-a-hole-test
  (let [requested (read-requested-values)]
    (testing "BareDOM's request events are read"
      (is (contains? requested "page-change-request")))
    (doseq [[template-name blog-template] templates/by-name
            {:keys [split]}               (templates/nodes blog-template)
            :when                         split]
      (testing (str template-name ": a part that listens to a request has a hole for what it asks")
        (is (empty? (unanswered requested split)))))))
