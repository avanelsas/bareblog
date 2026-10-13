(ns bareblog.read.image
  "The size of an image, read from the bytes of its file."
  (:require [clojure.string :as str]))

(defn- numbers-at
  "The `n` bytes of `bs` from `i`, as numbers from 0 to 255. Nil when `bs` ends before them."
  [^bytes bs i n]
  (when (<= 0 i (+ i n) (alength bs))
    (mapv (fn [k] (bit-and (aget bs k) 0xff)) (range i (+ i n)))))

(defn- number
  "The number in the `n` bytes of `bs` from `i`, with its `:big` or its `:small` end first.
   Nil when `bs` ends before them."
  [bs order i n]
  (when-some [numbers (numbers-at bs i n)]
    (reduce (fn [value x] (+ (* 256 value) x))
            0
            (cond-> numbers (= :small order) rseq))))

(defn- numbers-of
  "The numbers that `mark` stands for: text is the number of each of its characters."
  [mark]
  (if (string? mark) (mapv int mark) mark))

(defn- marked?
  "True when `bs` holds `mark` at `at`."
  [bs [at mark]]
  (= (numbers-of mark) (numbers-at bs at (count mark))))

;; The formats that keep their size at a fixed place.

(def ^:private fixed-formats
  "For each format: the marks that tell it, each a place and what is there, and where its width
   and its height are. A number may have bits to keep and an amount to add."
  [{:format :png
    :marks  [[0 [0x89 0x50 0x4e 0x47 0x0d 0x0a 0x1a 0x0a]]]
    :width  {:at 16 :bytes 4 :order :big}
    :height {:at 20 :bytes 4 :order :big}}
   {:format :gif
    :marks  [[0 "GIF8"]]
    :width  {:at 6 :bytes 2 :order :small}
    :height {:at 8 :bytes 2 :order :small}}
   {:format :webp-lossy
    :marks  [[0 "RIFF"] [8 "WEBP"] [12 "VP8 "]]
    :width  {:at 26 :bytes 2 :order :small :keep 0x3fff}
    :height {:at 28 :bytes 2 :order :small :keep 0x3fff}}
   {:format :webp-extended
    :marks  [[0 "RIFF"] [8 "WEBP"] [12 "VP8X"]]
    :width  {:at 24 :bytes 3 :order :small :add 1}
    :height {:at 27 :bytes 3 :order :small :add 1}}])

(defn- placed
  "The number that `place` describes in `bs`, or nil when `bs` ends before it."
  [bs {:keys [at bytes order keep add] :or {keep -1 add 0}}]
  (some-> (number bs order at bytes) (bit-and keep) (+ add)))

(defn- fixed-size
  "The size of `bs` as the fixed format `fixed`, or nil when `bs` is not that format."
  [{:keys [marks width height]} bs]
  (when (every? (partial marked? bs) marks)
    {:width (placed bs width) :height (placed bs height)}))

;; A lossless WebP packs its width and its height into four bytes, fourteen bits each, and
;; each one less than it is.

(def ^:private lossless-marks [[0 "RIFF"] [8 "WEBP"] [12 "VP8L"]])

(defn- lossless-size [bs]
  (when (every? (partial marked? bs) lossless-marks)
    (when-some [bits (number bs :small 21 4)]
      {:width  (inc (bit-and 0x3fff bits))
       :height (inc (bit-and 0x3fff (bit-shift-right bits 14)))})))

;; A JPEG is a run of segments. One of them holds the size, and one may say that the picture
;; is shown turned, which swaps its width and height.

(def ^:private jpeg-marks [[0 [0xff 0xd8]]])

(def ^:private frame-markers
  "The markers of the segments that hold the size of the picture."
  #{0xc0 0xc1 0xc2 0xc3 0xc5 0xc6 0xc7 0xc9 0xca 0xcb 0xcd 0xce 0xcf})

(def ^:private scan-marker 0xda)
(def ^:private exif-marker 0xe1)
(def ^:private orientation-tag 0x0112)

(defn- stand-alone?
  "True when a segment with `marker` is its marker and nothing more."
  [marker]
  (or (<= 0xd0 marker 0xd7) (= 0x01 marker)))

(defn- segments
  "The segments of the JPEG `bs` before its picture data, each with its marker and where it
   starts. They end where `bs` holds no whole start of a segment."
  [bs]
  (loop [i 2 found []]
    (let [[mark marker high low] (numbers-at bs i 4)]
      (cond
        (not= 0xff mark)       found
        (= scan-marker marker) found
        (= 0xff marker)        (recur (inc i) found)
        (stand-alone? marker)  (recur (+ i 2) found)
        :else                  (recur (+ i 2 (* 256 high) low)
                                      (conj found {:marker marker :at i}))))))

(defn- frame-size [bs {:keys [at]}]
  {:width (number bs :big (+ at 7) 2) :height (number bs :big (+ at 5) 2)})

(defn- exif-entry
  "The tag and the value of entry `n` of the directory at `directory`, read in `order`."
  [bs order directory n]
  [(number bs order (+ directory 2 (* 12 n)) 2)
   (number bs order (+ directory 10 (* 12 n)) 2)])

(defn- value-of [tag [entry-tag value]]
  (when (= tag entry-tag)
    value))

(defn- orientation
  "How the picture is shown turned, as the Exif segment at `at` says it: a number from 1 to 8,
   or nil when the segment does not say."
  [bs {:keys [at]}]
  (let [tiff  (+ at 10)
        order (if (marked? bs [tiff "MM"]) :big :small)]
    (when (marked? bs [(+ at 4) "Exif"])
      (when-some [directory (some->> (number bs order (+ tiff 4) 4) (+ tiff))]
        (some (partial value-of orientation-tag)
              (map (partial exif-entry bs order directory)
                   (range (or (number bs order directory 2) 0))))))))

(defn- on-its-side?
  "True when a picture with `orientation` is shown turned a quarter."
  [orientation]
  (<= 5 orientation 8))

(defn- turned?
  "True when one of `segments` says that the picture is shown turned a quarter."
  [bs segments]
  (boolean (some on-its-side?
                 (keep (partial orientation bs)
                       (filter (comp #{exif-marker} :marker) segments)))))

(defn- swapped [{:keys [width height]}]
  {:width height :height width})

(defn- jpeg-size [bs]
  (when (every? (partial marked? bs) jpeg-marks)
    (let [found (segments bs)]
      (when-some [frame (first (filter (comp frame-markers :marker) found))]
        (cond-> (frame-size bs frame)
          (turned? bs found) swapped)))))

;; The size of an SVG is the width and height of its svg element, or else the last two
;; numbers of its viewBox. The element is near the start, so only the start is read as text.

(def ^:private svg-start 4096)

(defn- svg-tag [^bytes bs]
  (re-find #"(?s)<svg\b[^>]*>" (String. bs 0 (int (min svg-start (alength bs))) "UTF-8")))

(defn- attribute [tag attribute-name]
  (second (re-find (re-pattern (str "\\s" attribute-name "\\s*=\\s*[\"']([^\"']*)[\"']")) tag)))

(defn- pixels
  "The number in `text` when it is a number of pixels, or nil."
  [text]
  (some->> text (re-matches #"\s*([0-9]*\.?[0-9]+)(px)?\s*") second parse-double))

(defn- view-box-size [tag]
  (let [numbers (some-> (attribute tag "viewBox") str/trim (str/split #"[\s,]+"))]
    (when (= 4 (count numbers))
      {:width (parse-double (nth numbers 2)) :height (parse-double (nth numbers 3))})))

(defn- svg-size [bs]
  (when-some [tag (svg-tag bs)]
    (let [width  (pixels (attribute tag "width"))
          height (pixels (attribute tag "height"))]
      (if (and width height)
        {:width width :height height}
        (view-box-size tag)))))

(def ^:private readers
  "A function for each format, from bytes to a size or nil. They are tried in order."
  (conj (mapv (fn [fixed] (partial fixed-size fixed)) fixed-formats)
        lossless-size
        jpeg-size
        svg-size))

(defn- real
  "The `size` when it has a width and a height above nothing, or nil."
  [{:keys [width height] :as size}]
  (when (and (number? width) (number? height) (pos? width) (pos? height))
    size))

(defn- size-with [bs read-size]
  (real (read-size bs)))

(defn size
  "The width and the height of the image in the bytes `bs`, as it is shown. Nil when the bytes
   are no PNG, GIF, JPEG, WebP or SVG, or end before their size."
  [bs]
  (some (partial size-with bs) readers))
