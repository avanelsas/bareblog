(ns bareblog.test-runner
  "Runs the tests of the pure code in babashka."
  (:require [clojure.test :as t]))

(def namespaces
  '[bareblog.address-test
    bareblog.claude-test
    bareblog.cli-test
    bareblog.example-test
    bareblog.feed-test
    bareblog.generate-test
    bareblog.header-test
    bareblog.image-test
    bareblog.output-test
    bareblog.plan-test
    bareblog.publish-test
    bareblog.reader-test
    bareblog.serve-test
    bareblog.site-test
    bareblog.source-test
    bareblog.start-test
    bareblog.text-test
    bareblog.watch-test
    bareblog.app.blocks-test
    bareblog.app.state-test
    bareblog.app.templates-test
    bareblog.app.view-test])

(defn run
  "Runs every test namespace. The process ends with 1 when a test fails."
  []
  (apply require namespaces)
  (let [{:keys [fail error]} (apply t/run-tests namespaces)]
    (when (pos? (+ fail error))
      (System/exit 1))))
