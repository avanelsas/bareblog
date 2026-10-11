(ns bareblog.test-runner
  "Runs the tests of the pure code in babashka."
  (:require [clojure.test :as t]))

(def namespaces
  '[bareblog.header-test
    bareblog.post-test
    bareblog.reader-test
    bareblog.serve-test
    bareblog.app.blocks-test
    bareblog.app.templates-test
    bareblog.app.view-test])

(defn run
  "Runs every test namespace. The process ends with 1 when a test fails."
  []
  (apply require namespaces)
  (let [{:keys [fail error]} (apply t/run-tests namespaces)]
    (when (pos? (+ fail error))
      (System/exit 1))))
