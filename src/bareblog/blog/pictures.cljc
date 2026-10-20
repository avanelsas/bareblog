(ns bareblog.blog.pictures
  "The pictures of a blog: the files beside its settings that the settings name.

   A picture is of a kind, which is the setting that names it. A logo and a favicon have a
   picture for each mode of the theme, `:light` and `:dark`, and the settings name them as a
   map of the two, or as one name for both. Each named picture is where it is among the
   pictures of the blog, under `:at`, and its file name, under `:src`."
  (:require [bareblog.blog.layout :as layout]
            [bareblog.blog.text :as text]
            [clojure.string :as str]))

(def kinds
  "Each kind of picture, by the setting that names it: the extensions of the formats it can
   be, how they are said, and whether it has a picture for each mode of the theme."
  (array-map
   :link-preview {:extensions #{"png" "jpg" "jpeg" "gif"}
                  :said       "a PNG, JPEG or GIF"}
   :logo         {:extensions #{"svg" "png" "jpg" "jpeg" "gif" "webp"}
                  :said       "an SVG, PNG, JPEG, GIF or WebP"
                  :modes?     true}
   :favicon      {:extensions #{"svg" "png" "ico"}
                  :said       "an SVG, PNG or ICO"
                  :modes?     true}))

(def ^:private modes #{:light :dark})

(defn- by-mode
  "The pictures that `value` names for each mode: one name is the picture of both."
  [value]
  (if (string? value) {:light value} value))

(defn- by-mode?
  "True when `value` names a picture for each mode: a name, or a map with one under `:light`,
   and maybe one under `:dark`."
  [value]
  (or (string? value)
      (and (map? value) (contains? value :light) (every? modes (keys value)))))

(defn- of-kind
  "Each picture that `value` names, as the setting of the kind `kind`."
  [kind value]
  (if (:modes? (kinds kind))
    (when (by-mode? value)
      (for [[mode src] (by-mode value)]
        {:at [kind mode] :src src}))
    [{:at [kind] :src value}]))

(defn- setting
  "The kind `kind` with the value that `settings` give it, or nil when they give none."
  [settings kind]
  (when-some [value (get settings kind)]
    [kind value]))

(defn named
  "Each picture that `settings` name, in the order of the kinds."
  [settings]
  (mapcat (partial apply of-kind) (keep (partial setting settings) (keys kinds))))

(defn- extension [file-name]
  (str/lower-case (peek (str/split file-name #"\." -1))))

(defn fits?
  "True when the file name of `picture` can name a picture of its kind beside the settings."
  [{:keys [at src]}]
  (boolean (and (string? src)
                (text/file-name? src)
                (str/includes? src ".")
                (contains? (:extensions (kinds (first at))) (extension src)))))

(defn kind-of
  "The kind of `picture`, which is the setting that names it."
  [{:keys [at]}]
  (first at))

(defn said
  "How the formats of a picture of the kind of `picture` are said."
  [picture]
  (:said (kinds (kind-of picture))))

(defn settings-problems
  "What is wrong with how `settings` name a picture for each mode, as texts."
  [settings]
  (for [[kind {:keys [modes?]}] kinds
        :let                    [value (get settings kind)]
        :when                   (and modes? (some? value) (not (by-mode? value)))]
    (str layout/settings-file " gives " (pr-str value) " as its " kind ", which is the name of "
         "a picture, or a map with one under :light and maybe one under :dark.")))

(defn files
  "The file name of each picture in `pictures`, the pictures of a blog as they were read."
  [pictures]
  (distinct (keep :src (tree-seq map? vals pictures))))
