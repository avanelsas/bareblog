(ns bareblog.image-test
  (:require [bareblog.image :as image]
            [clojure.test :refer [deftest is testing]]))

;; A file is built from pieces: text, numbers from 0 to 255, and the results of the helpers
;; below, which spell a number out in bytes.

(defn- big
  "The `n` bytes of `x`, with its big end first."
  [n x]
  (mapv (fn [k] (bit-and 0xff (bit-shift-right x (* 8 (- n k 1))))) (range n)))

(defn- small
  "The `n` bytes of `x`, with its small end first."
  [n x]
  (vec (rseq (big n x))))

(defn- numbers [piece]
  (cond
    (nil? piece)        []
    (string? piece)     (map int piece)
    (sequential? piece) (mapcat numbers piece)
    :else               [piece]))

(defn- file [& pieces]
  (byte-array (map unchecked-byte (numbers pieces))))

(defn- cut
  "The file of `pieces` without its last `n` bytes."
  [n & pieces]
  (byte-array (drop-last n (apply file pieces))))

(def ^:private png
  [0x89 "PNG" 0x0d 0x0a 0x1a 0x0a (big 4 13) "IHDR" (big 4 300) (big 4 200)])

(def ^:private gif
  ["GIF89a" (small 2 300) (small 2 200)])

(defn- segment
  "A segment of a JPEG with `marker` and `content`. Its length counts its own two bytes."
  [marker & content]
  [0xff marker (big 2 (+ 2 (count (numbers content)))) content])

(def ^:private frame
  (segment 0xc0 8 (big 2 200) (big 2 300) 3))

(defn- exif
  "An Exif segment that says the picture is shown with `orientation`, with numbers written by
   `end`, which is `big` or `small`."
  [end orientation]
  (segment 0xe1
           "Exif" 0 0
           (if (= end big) "MM" "II") (end 2 42) (end 4 8)
           (end 2 1)
           (end 2 0x0112) (end 2 3) (end 4 1) (end 2 orientation) 0 0
           (end 4 0)))

(defn- jpeg [& segments]
  [0xff 0xd8 (segment 0xe0 "JFIF" 0) segments])

(defn- webp [kind & content]
  ["RIFF" (small 4 0) "WEBP" kind (small 4 0) content])

(deftest png-test
  (is (= {:width 300 :height 200} (image/size (file png)))))

(deftest gif-test
  (is (= {:width 300 :height 200} (image/size (file gif))))
  (is (= {:width 300 :height 200} (image/size (file "GIF87a" (small 2 300) (small 2 200))))))

(deftest jpeg-test
  (testing "the size is in the frame segment, after the segments before it"
    (is (= {:width 300 :height 200} (image/size (file (jpeg frame))))))
  (testing "a picture that is shown turned a quarter has its width and height the other way round"
    (is (= {:width 200 :height 300} (image/size (file (jpeg (exif small 6) frame)))))
    (is (= {:width 200 :height 300} (image/size (file (jpeg (exif big 8) frame))))))
  (testing "a picture that is shown upright or upside down keeps its width and height"
    (is (= {:width 300 :height 200} (image/size (file (jpeg (exif small 1) frame)))))
    (is (= {:width 300 :height 200} (image/size (file (jpeg (exif big 3) frame))))))
  (testing "a file with no frame segment before its picture data has no size"
    (is (nil? (image/size (file (jpeg)))))
    (is (nil? (image/size (file (jpeg (segment 0xda) frame)))))))

(deftest webp-test
  (testing "a lossy file"
    (is (= {:width 300 :height 200}
           (image/size (file (webp "VP8 " 0 0 0 0x9d 0x01 0x2a (small 2 300) (small 2 200)))))))
  (testing "a lossless file holds each of them less one, in fourteen bits"
    (is (= {:width 300 :height 200}
           (image/size (file (webp "VP8L" 0x2f (small 4 (+ 299 (bit-shift-left 199 14)))))))))
  (testing "an extended file holds each of them less one"
    (is (= {:width 640 :height 360}
           (image/size (file (webp "VP8X" 0 0 0 0 (small 3 639) (small 3 359))))))))

(deftest svg-test
  (testing "the width and height of the svg element"
    (is (= {:width 120.0 :height 80.0}
           (image/size (file "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"120\" height=\"80px\"></svg>")))))
  (testing "with no width and height, the size of the viewBox"
    (is (= {:width 800.0 :height 450.0}
           (image/size (file "<?xml version=\"1.0\"?>\n<svg viewBox=\"0 0 800 450\" role=\"img\"><path stroke-width=\"3\"/></svg>")))))
  (testing "a width that is no number of pixels is not a width"
    (is (= {:width 16.0 :height 9.0}
           (image/size (file "<svg width=\"100%\" height=\"100%\" viewBox=\"0 0 16 9\"></svg>")))))
  (testing "the numbers of a viewBox may have commas between them"
    (is (= {:width 10.5 :height 4.0} (image/size (file "<svg viewBox='0,0,10.5,4'></svg>")))))
  (testing "an svg element with neither has no size"
    (is (nil? (image/size (file "<svg xmlns=\"http://www.w3.org/2000/svg\"></svg>"))))))

(deftest no-size-test
  (testing "bytes that are no image have no size"
    (is (nil? (image/size (file "Just some text."))))
    (is (nil? (image/size (file)))))
  (testing "a file that ends inside its size has no size, however little is missing"
    (is (nil? (image/size (cut 1 png))))
    (is (nil? (image/size (cut 1 gif))))
    (is (nil? (image/size (cut 1 (jpeg (segment 0xc0 8 (big 2 200) (big 2 300)))))))
    (is (nil? (image/size (cut 1 (webp "VP8X" 0 0 0 0 (small 3 639) (small 3 359))))))
    (is (nil? (image/size (cut 2 (webp "VP8L" 0x2f (small 4 (+ 299 (bit-shift-left 199 14))))))))))
