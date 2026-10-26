(ns bareblog.blog.font
  "The font of a blog: the family that its settings name under `:font`, for the running text of
   its posts and its lists.

   A family that has a folder among the fonts beside the settings is served from there, so every
   reader sees it. The folder holds the files of the font and a stylesheet of its faces, which
   names them from the folder. A family without a folder is shown to a reader who has it
   installed, and the theme's font to any other."
  (:require [bareblog.blog.layout :as layout]
            [clojure.string :as str]))

(def faces-file
  "The file in the folder of a font that holds its faces, as a stylesheet."
  "font.css")

(def ^:private family-pattern
  #"[A-Za-z0-9][A-Za-z0-9 ._-]*")

(defn family?
  "True when `value` can name a family of fonts in the settings."
  [value]
  (and (string? value) (some? (re-matches family-pattern value))))

(defn family
  "The family of fonts that `settings` name, or nil."
  [settings]
  (let [value (:font settings)]
    (when (family? value)
      value)))

(defn settings-problems
  "What is wrong with how `settings` name a font, as texts."
  [settings]
  (when-some [value (get settings :font)]
    (when-not (family? value)
      [(str layout/settings-file " gives " (pr-str value) " as its :font, which is the name of a "
            "font in quotes, of letters, digits, spaces, dots, dashes and underscores.")])))

(defn slug
  "The name of the folder of the family `family`: its letters and digits in lower case, with a
   dash for what is between them."
  [family]
  (-> (str/lower-case family)
      (str/replace #"[^a-z0-9]+" "-")
      (str/replace #"^-|-$" "")))

(defn folder
  "The folder of the family `family`, from the top of the blog."
  [family]
  (str layout/fonts-dir "/" (slug family)))

;; A face names its files from the folder of the font. The stylesheet of the blog is at its
;; top, so each such name is named from there.
(defn- located-url
  [folder [whole quote target]]
  (if (re-find #"^(?:[a-z]+:|/|#)" target)
    whole
    (str "url(" quote folder "/" target quote ")")))

(defn located
  "The stylesheet `css`, whose files are named from the folder `folder`, with each file named
   from the top of the blog."
  [css folder]
  (str/replace css #"url\(\s*(['\"]?)([^'\")]+)\1\s*\)" (partial located-url folder)))

(defn stylesheet
  "The stylesheet of the font of a blog, for the family `family`, with the faces `faces` that its
   folder holds, or nil when it has none. Empty when there is no family."
  [family faces]
  (if (nil? family)
    ""
    (str (some-> faces (located (folder family)) (str "\n"))
         ":root {\n  --font: \"" family "\";\n}\n")))
