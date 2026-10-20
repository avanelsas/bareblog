(ns bareblog.app.diary
  "The Diary blog template: Journal, with a calendar beside the list of posts, where a reader
   picks a day or a month.

   See `bareblog.app.templates` for what a blog template is."
  (:require [bareblog.app.journal :as journal]
            [bareblog.app.pages :as pages]
            [bareblog.blog.days :as days]
            [clojure.string :as str]))

;; A date is a day, as `2026-10-12`, or a month, as `2026-10`, so its length tells them apart.
(defn- chosen-day
  "The day that the reader chose, or nil when they chose a month or nothing."
  [{:keys [date]}]
  (when (= 10 (count date))
    date))

(defn- shown-month
  "The month that the calendar shows: that of the date the reader chose, or else that of the
   newest post."
  [{:keys [date days]}]
  (some-> (or date (peek days)) (subs 0 7)))

(defn- first-day [{:keys [days]}]
  (first days))

(defn- last-day [{:keys [days]}]
  (peek days))

;; `x-calendar` marks no days. It is held to the days of the posts, and every day between them
;; that has no post cannot be picked.
(defn- quiet-days
  "The days between the first post and the newest that have no post, as `x-calendar` takes them."
  [{:keys [days]}]
  (str/join " " (days/days-without days)))

(defn- no-calendar?
  "True where a calendar would mislead: on the page of a tag, where a date picks nothing, and
   in a blog with no post that was published, which has no day to pick."
  [{:keys [heading days]}]
  (boolean (or heading (empty? days))))

(def ^:private calendar
  "The calendar of the days with a post. Picking a day or moving to a month chooses it."
  [:x-calendar {:class          "calendar"
                :value          chosen-day
                :month          shown-month
                :min            first-day
                :max            last-day
                :disabled-dates quiet-days
                :locale         :language
                :hidden         no-calendar?
                :on             {"x-calendar-change"   :day
                                 "x-calendar-navigate" :month}}])

(def ^:private days-and-posts
  "The node of a page that lists posts, beside the calendar."
  (pages/listing :diary-entries
                 [:div {:class "days-and-posts"}
                  calendar
                  [:ol {:class "diary-entries" :data-x-part "entries"}]]
                 pages/entry))

(def template
  "The node of the whole document."
  (journal/shell :diary (assoc pages/page-rows "entries" days-and-posts)))
