(ns bareblog.test-runner
  "Runs the tests of the pure code in babashka."
  (:require [clojure.test :as t]))

(def namespaces
  '[bareblog.code-test
    bareblog.example-test
    bareblog.blog.address-test
    bareblog.blog.days-test
    bareblog.blog.pictures-test
    bareblog.blog.shape-test
    bareblog.blog.site-test
    bareblog.blog.tags-test
    bareblog.blog.text-test
    bareblog.app.blocks-test
    bareblog.app.card-test
    bareblog.app.live-test
    bareblog.app.requests-test
    bareblog.app.state-test
    bareblog.app.templates-test
    bareblog.app.view-test
    bareblog.written.feed-test
    bareblog.written.output-test
    bareblog.read.image-test
    bareblog.read.reader-test
    bareblog.read.source-test
    bareblog.preview.serve-test
    bareblog.preview.watch-test
    bareblog.publish.generate-test
    bareblog.publish.git-test
    bareblog.publish.plan-test
    bareblog.publish.run-test
    bareblog.command.cli-test
    bareblog.command.start-test
    bareblog.command.tags-test
    bareblog.command.update-test])

(defn run
  "Runs every test namespace. The process ends with 1 when a test fails."
  []
  (apply require namespaces)
  (let [{:keys [fail error]} (apply t/run-tests namespaces)]
    (when (pos? (+ fail error))
      (System/exit 1))))
