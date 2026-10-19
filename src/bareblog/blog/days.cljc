(ns bareblog.blog.days
  "The days of a calendar, each written as `2026-10-12`."
  (:require [clojure.string :as str]))

;; A day is counted from 1970-01-01, as in the civil calendar of Howard Hinnant, so that the
;; days between two days are a range of numbers.
(defn- whole-number [digits]
  #?(:clj (Long/parseLong digits) :cljs (js/parseInt digits 10)))

(defn- day-number
  "The number of the day `day`, written as `2026-10-12`."
  [day]
  (let [[y m d] (map whole-number (str/split day #"-"))
        y       (if (<= m 2) (dec y) y)
        era     (quot y 400)
        yoe     (- y (* era 400))
        doy     (+ (quot (+ (* 153 (mod (+ m 9) 12)) 2) 5) (dec d))
        doe     (+ (* yoe 365) (quot yoe 4) (- (quot yoe 100)) doy)]
    (+ (* era 146097) doe -719468)))

(defn- two-digits [n]
  (subs (str (+ 100 n)) 1))

(defn- number-day
  "The day with the number `n`, written as `2026-10-12`."
  [n]
  (let [z   (+ n 719468)
        era (quot z 146097)
        doe (- z (* era 146097))
        yoe (quot (- doe (quot doe 1460) (- (quot doe 36524)) (quot doe 146096)) 365)
        doy (- doe (+ (* 365 yoe) (quot yoe 4) (- (quot yoe 100))))
        mp  (quot (+ (* 5 doy) 2) 153)
        d   (inc (- doy (quot (+ (* 153 mp) 2) 5)))
        m   (if (< mp 10) (+ mp 3) (- mp 9))
        y   (+ yoe (* era 400) (if (<= m 2) 1 0))]
    (str y "-" (two-digits m) "-" (two-digits d))))

(defn days-without
  "The days from the first of `days` to the last that are not among them, in order. `days`
   are in order."
  [days]
  (when (seq days)
    (let [had (set days)]
      (remove had (map number-day (range (day-number (first days))
                                         (inc (day-number (peek days)))))))))
