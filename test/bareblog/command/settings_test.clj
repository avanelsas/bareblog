(ns bareblog.command.settings-test
  (:require [bareblog.command.settings :as settings]
            [clojure.edn :as edn]
            [clojure.test :refer [deftest is testing]]))

(deftest with-setting-test
  (testing "a setting is added at the end, and what the author wrote stays"
    (is (= "{:title \"x\" ;; mine\n :comments {:script \"https://a\" :on? false}\n}"
           (settings/with-setting "{:title \"x\" ;; mine\n}" :comments {:script "https://a" :on? false}))))
  (testing "a setting that is there takes the new value in its place"
    (is (= {:title "x" :comments {:script "https://b" :on? true}}
           (edn/read-string (settings/with-setting "{:comments {:script \"https://a\" :on? false} :title \"x\"}"
                                                   :comments {:script "https://b" :on? true}))))))

(deftest without-setting-test
  (testing "a setting is taken away, and a setting that is not there leaves the text as it was"
    (is (= "{:title \"x\"}" (settings/without-setting "{:title \"x\" :comments {:on? true}}" :comments)))
    (is (= "{:title \"x\"}" (settings/without-setting "{:title \"x\"}" :comments)))))
