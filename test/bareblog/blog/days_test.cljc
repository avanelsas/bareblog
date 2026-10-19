(ns bareblog.blog.days-test
  (:require [bareblog.blog.days :as days]
            [clojure.test :refer [deftest is testing]]))

(deftest days-without-test
  (testing "the days without a post run from the first day to the last, across a leap day and a year"
    (is (= ["2024-02-28" "2024-02-29" "2024-03-01"] (days/days-without ["2024-02-27" "2024-03-02"])))
    (is (= ["2025-12-31"] (days/days-without ["2025-12-30" "2026-01-01"])))
    (is (= 673 (count (days/days-without ["2024-02-27" "2026-01-01"])))))
  (testing "no days, or days next to each other, have none without"
    (is (empty? (days/days-without [])))
    (is (empty? (days/days-without ["2026-10-01" "2026-10-02"])))))
