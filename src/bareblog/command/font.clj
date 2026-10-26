(ns bareblog.command.font
  "The `bareblog font` command: the font of a blog, a font of Google Fonts fetched into it, and
   the font taken away again.

   The edge of the command. What a font of a blog is, is `bareblog.blog.font`, which is pure, and
   so is what is read of Google Fonts and how the settings change, at the top. Each function here
   is given how, with `:say`, and `:ask` that puts a question to the author and gives true for a
   yes. It gives the number that the process ends with."
  (:require [babashka.fs :as fs]
            [babashka.http-client :as http]
            [bareblog.blog.font :as font]
            [bareblog.blog.layout :as layout]
            [bareblog.read.source :as source]
            [cheshire.core :as json]
            [clojure.string :as str]
            [rewrite-clj.node :as node]
            [rewrite-clj.zip :as z]))

;; What is read of Google Fonts. Pure.

(def ^:private catalogue-address
  "The address of the list of every family of Google Fonts, with its axes and its styles."
  "https://fonts.google.com/metadata/fonts")

(def ^:private faces-address
  "The address of the stylesheet of the faces of a family, before what it asks for."
  "https://fonts.googleapis.com/css2?display=swap&family=")

(def ^:private licence-address
  "The address of the directories of the families of Google Fonts, by their licence."
  "https://raw.githubusercontent.com/google/fonts/main/")

(def ^:private licences
  "Where a family of Google Fonts keeps its licence: the directory of the licence, and its file."
  [["ofl" "OFL.txt"] ["apache" "LICENSE.txt"] ["ufl" "UFL.txt"]])

;; Google Fonts sends a browser that it knows the faces as WOFF2, one for each part of the
;; letters, which a browser fetches only for a page that has letters of it.
(def ^:private browser
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0 Safari/537.36")

(def ^:private axes
  "The axes of a family that the running text of a blog uses."
  #{"opsz" "wght"})

(def ^:private weights
  "The weights of a family without a weight axis that the running text of a blog uses."
  #{400 700})

(defn- weight-of
  "The weight of a style of the catalogue, as `400` of `400i`."
  [style]
  (some-> (re-find #"^\d+" style) parse-long))

(defn- italic? [style]
  (str/ends-with? style "i"))

(defn- family-of
  "A family of the catalogue, as `entry` gives it: its name, the axes that a blog uses, and
   its weights and whether it has italics."
  [{:keys [family fonts] :as entry}]
  (let [styles (map name (keys fonts))]
    {:family  family
     :axes    (vec (sort-by :tag (filter (comp axes :tag) (:axes entry))))
     :weights (into (sorted-set) (keep weight-of) styles)
     :italic? (boolean (some italic? styles))}))

(defn families
  "Each family of the catalogue of Google Fonts, from its text `text`."
  [text]
  (let [read (json/parse-string (subs text (str/index-of text "{")) true)]
    (mapv family-of (:familyMetadataList read))))

(defn- distance
  "How many letters must change to make the text `a` the text `b`."
  [a b]
  (peek (reduce (fn [row [i x]]
                  (reduce (fn [next [k y]]
                            (conj next (min (inc (peek next))
                                            (inc (row (inc k)))
                                            (+ (row k) (if (= x y) 0 1)))))
                          [(inc i)]
                          (map-indexed vector b)))
                (vec (range (inc (count b))))
                (map-indexed vector a))))

(defn- like?
  "True when the family `family` is named like `asked`, whose letters are `letters`: it holds
   them, or is two letters or fewer from them."
  [letters {:keys [family]}]
  (let [named (str/lower-case family)]
    (or (str/includes? named letters) (<= (distance named letters) 2))))

(defn found
  "The family among `all` that `asked` names, as `:family`, or the `:problem` when none does."
  [all asked]
  (let [letters (str/lower-case (str/trim asked))]
    (if-some [family (first (filter (comp #{letters} str/lower-case :family) all))]
      {:family family}
      (let [alike (take 5 (map :family (filter (partial like? letters) all)))]
        {:problem (str "Google Fonts has no font named " (pr-str asked) "."
                       (when (seq alike)
                         (str " It has " (str/join ", " (map pr-str alike)) ".")))}))))

(defn- number [n]
  (if (== n (long n)) (str (long n)) (str n)))

(defn- range-of [{:keys [min max]}]
  (if (== min max) (number min) (str (number min) ".." (number max))))

(defn faces-query
  "What the address of the faces of `family` asks for: every value of the axes that a blog
   uses where it has them, or else the weights that a blog uses, in roman and italic."
  [{:keys [family axes italic?] used-weights :weights}]
  (let [variable? (seq axes)
        tags      (if variable? (map :tag axes) ["wght"])
        values    (if variable?
                    [(str/join "," (map range-of axes))]
                    (map str (or (seq (filter weights used-weights)) (take 1 used-weights))))]
    (str (str/replace family " " "+") ":"
         (str/join "," (cond->> tags italic? (cons "ital")))
         "@" (str/join ";" (for [ital (if italic? ["0," "1,"] [""]) value values]
                             (str ital value))))))

(defn faces-address-of
  "The address of the stylesheet of the faces of `family`."
  [family]
  (str faces-address (faces-query family)))

(def ^:private face-pattern
  #"(?:/\* ([\w-]+) \*/\s*)?@font-face \{([^}]*)\}")

(defn- property [declarations property-name]
  (some-> (re-find (re-pattern (str property-name ":\\s*([^;]+);")) declarations) second str/trim))

(defn- file-name
  "The name of the file of a face of the family in `slug`, of the part `part` of its letters."
  [slug style weight part]
  (str/join "-" (remove nil? [slug (when-not (str/includes? weight " ") weight) style part])))

(defn faces
  "Each face that the stylesheet `css` of Google Fonts names, for the family in the folder
   `slug`: its `:address`, and the `:file` it is kept in."
  [css slug]
  (for [[_ part declarations] (re-seq face-pattern css)
        :let                  [style  (property declarations "font-style")
                               weight (property declarations "font-weight")
                               src    (property declarations "src")]]
    {:address (second (re-find #"url\(([^)]+)\)" src))
     :file    (str (file-name slug style weight (or part "all")) ".woff2")}))

(defn- kept-beside
  "The stylesheet `css` with the face at `:address` named by its `:file`."
  [css {:keys [address file]}]
  (str/replace css address file))

(defn faces-css
  "The stylesheet `css` of Google Fonts with each face named from beside it, as `faces` says
   where it is kept."
  [css faces]
  (reduce kept-beside css faces))

(defn licence-addresses
  "Where the licence of `family` may be, in the order it is looked for."
  [{:keys [family]}]
  (let [id (str/replace (str/lower-case family) #"[^a-z0-9]" "")]
    (for [[licence-dir file] licences]
      {:address (str licence-address licence-dir "/" id "/" file) :file file})))

;; How the settings change. Pure. A setting is changed in the text, so that what the author
;; wrote around it stays as it was.

(defn- ends-line? [n]
  (contains? #{:comment :newline} (node/tag n)))

(defn- space? [n]
  (contains? #{:whitespace :newline :comma} (node/tag n)))

(defn- newline? [n]
  (= :newline (some-> n node/tag)))

(defn- added
  "The nodes of the map `nodes` with the setting `entry` at its end: on a line of its own
   where the map ends with one, and after the last setting where it ends on the line of it."
  [nodes entry]
  (let [body     (vec (reverse (drop-while space? (reverse nodes))))
        trailing (subvec (vec nodes) (count body))
        indent   (node/spaces 1)
        new-line (node/newlines 1)]
    (cond
      (empty? body)             (concat entry trailing)
      (ends-line? (peek body))  (concat body [indent] entry [new-line] trailing)
      (some newline? trailing)  (concat body [new-line indent] entry trailing)
      :else                     (concat body [indent] entry trailing))))

(defn- blank? [n]
  (= :whitespace (node/tag n)))

(defn- blanks
  "How many of `nodes` in a row from the first are spaces on a line."
  [nodes]
  (count (take-while blank? nodes)))

(defn- taken
  "The nodes of the map `nodes` without the setting whose key is at `i` and its value at `j`,
   and without what set it apart: the spaces before it, with the end of the line before it
   where that is a new line, or the spaces after it where it is the first, or the end of its
   own line after a comment."
  [nodes i j]
  (let [start  (- i (blanks (rseq (subvec nodes 0 i))))
        before (get nodes (dec start))
        after  (subvec nodes (inc j))
        spaces (blanks after)]
    (concat (subvec nodes 0 (cond-> start (newline? before) dec))
            (cond
              (nil? before)                         (drop spaces after)
              (and (= :comment (node/tag before))
                   (newline? (get after spaces)))  (drop (inc spaces) after)
              :else                                 after))))

(defn- setting? [n]
  (not (or (space? n) (= :comment (node/tag n)))))

(defn- font-key? [n]
  (and (= :token (node/tag n)) (= :font (node/sexpr n))))

(defn- index-of
  "The index of the first of `nodes` from `from` that `pred` holds for, or nil."
  [pred nodes from]
  (some (fn [i] (when (pred (nodes i)) i)) (range from (count nodes))))

(defn- font-at
  "The index of the key and of the value of the font among the nodes of the map `nodes`, or nil."
  [nodes]
  (when-some [i (index-of font-key? nodes 0)]
    [i (index-of setting? nodes (inc i))]))

(defn- set-font [family nodes]
  (let [value (node/token-node family)]
    (if-some [[_ j] (font-at nodes)]
      (assoc nodes j value)
      (added nodes [(node/keyword-node :font) (node/spaces 1) value]))))

(defn- take-font [nodes]
  (if-some [[i j] (font-at nodes)]
    (taken nodes i j)
    nodes))

(defn- rewritten
  "The text of the settings `text` with the nodes of its map as `change` makes them of those it
   has."
  [text change]
  (let [settings (z/of-string text)
        map-node (z/node settings)
        changed  (change (vec (node/children map-node)))]
    (z/root-string (z/replace settings (node/replace-children map-node changed)))))

(defn with-font
  "The text of the settings `text` with `family` as the font of the blog."
  [text family]
  (rewritten text (partial set-font family)))

(defn without-font
  "The text of the settings `text` with no font of the blog."
  [text]
  (rewritten text take-font))

;; Doing it.

(defn- read-settings-text [dir]
  (slurp (fs/file dir layout/settings-file)))

(defn- write-settings! [dir text]
  (spit (fs/file dir layout/settings-file) text))

(defn- kilobytes [path]
  (quot (+ 1023 (fs/size path)) 1024))

(defn list!
  "Says the font of the blog in the directory `dir`, and whether it is served."
  [{:keys [say]} dir]
  (let [settings (source/read-settings dir)
        family   (font/family settings)
        problems (font/settings-problems settings)]
    (cond
      (seq problems)
      (run! say problems)

      (nil? family)
      (say "The blog has the font of its theme. Give it one with: bareblog font \"Literata\"")

      (fs/directory? (fs/path dir (font/folder family)))
      (say (str "The font of the blog is " family ", served from " (font/folder family) "."))

      :else
      (say (str "The font of the blog is " family ", which a reader sees who has it installed. "
                "Serve it to every reader with: bareblog font " (pr-str family))))
    0))

(defn- fetch-text [address]
  (:body (http/get address {:headers {"User-Agent" browser}})))

(defn- fetch-file! [address path]
  (fs/write-bytes path (:body (http/get address {:headers {"User-Agent" browser} :as :bytes}))))

(defn- fetch-licence!
  "Fetches the licence of `family` into the directory `into`, and gives its file, or nil when
   it is not found."
  [family into]
  (some (fn [{:keys [address file]}]
          (let [response (http/get address {:throw false :as :bytes})]
            (when (= 200 (:status response))
              (fs/write-bytes (fs/path into file) (:body response))
              file)))
        (licence-addresses family)))

(defn- fetch-font!
  "Fetches the faces of `family` and its licence into the directory `into`, with the stylesheet
   of its faces. Gives the files of its faces, and the file of its licence or nil."
  [{:keys [family] :as found-family} into]
  (let [css   (fetch-text (faces-address-of found-family))
        all   (faces css (font/slug family))]
    (doseq [{:keys [address file]} all]
      (fetch-file! address (fs/path into file)))
    (spit (fs/file into font/faces-file)
          (str "/* " family ", from Google Fonts. */\n" (faces-css css all)))
    {:files (map :file all) :licence (fetch-licence! found-family into)}))

(defn- remove-old!
  "Removes the folder of the font `old` of the blog in `dir` once the author says yes, when it
   is not the folder of `family`."
  [{:keys [say ask]} dir old family]
  (let [old-folder (some-> old font/folder)]
    (when (and old-folder
               (not= old-folder (font/folder family))
               (fs/directory? (fs/path dir old-folder)))
      (if (ask (str "Remove " old-folder ", the font " old " that the blog had?"))
        (fs/delete-tree (fs/path dir old-folder))
        (say (str old-folder " stays."))))))

(defn- put-in-place!
  "Puts the directory `fetched` in the place of the folder of `family` in the blog in `dir`."
  [dir fetched family]
  (let [to (fs/path dir (font/folder family))]
    (fs/delete-tree to)
    (fs/create-dirs (fs/parent to))
    (fs/move fetched to)))

(defn- say-fetched!
  "Says what the blog in `dir` holds of `family` now, from the files and the licence that came
   of fetching it, and how it is published."
  [{:keys [say]} dir family {:keys [files licence]}]
  (let [to (fs/path dir (font/folder family))]
    (say (str (font/folder family) " holds " family ": " (count files) " files of "
              (reduce + (map (comp kilobytes (partial fs/path to)) files)) " KB, of which a reader "
              "fetches those of the letters a page has."))
    (when-not licence
      (say (str "Its licence was not found. Read it at https://fonts.google.com/specimen/"
                (str/replace family " " "+") " before you publish.")))
    (say "The preview shows it. It is published with: bareblog publish --all")))

;; The font is fetched beside the fonts of the blog, and takes the place of its folder once all
;; of it is there. A fetch that fails leaves the blog as it was.
(defn- fetched!
  "Fetches `family` of Google Fonts into the blog in the directory `dir`, and makes it the font
   of the blog, in place of the font `old`."
  [how dir old family]
  (let [fonts   (fs/create-dirs (fs/path dir layout/fonts-dir))
        fetched (fs/create-temp-dir {:dir fonts :prefix ".fetching-" :posix-file-permissions "rwxr-xr-x"})]
    (try
      (let [came (fetch-font! family fetched)
            name (:family family)]
        (remove-old! how dir old name)
        (put-in-place! dir fetched name)
        (write-settings! dir (with-font (read-settings-text dir) name))
        (say-fetched! how dir name came)
        0)
      (finally
        (fs/delete-tree fetched)))))

(defn- unreached [why problem]
  (str why (ex-message problem) " Nothing was changed."))

(defn fetch!
  "Fetches the font `asked` of Google Fonts into the blog in the directory `dir`, and makes it
   the font of the blog. The folder of the font it had before is removed once the author says
   yes."
  [{:keys [say] :as how} dir asked]
  (let [old (font/family (source/read-settings dir))]
    (try
      (let [{:keys [problem family]} (found (families (fetch-text catalogue-address)) asked)]
        (if problem
          (do (say problem) 1)
          (fetched! how dir old family)))
      (catch java.io.IOException problem
        (say (unreached "Could not reach Google Fonts: " problem))
        1)
      (catch clojure.lang.ExceptionInfo problem
        (if (:status (ex-data problem))
          (do (say (unreached "Google Fonts did not send the font: " problem)) 1)
          (throw problem))))))

(defn none!
  "Takes the font away from the blog in the directory `dir` once the author says yes, with its
   folder, so that the blog has the font of its theme."
  [{:keys [say ask]} dir]
  (let [settings (source/read-settings dir)
        folder   (some-> (font/family settings) font/folder)
        there?   (and folder (fs/directory? (fs/path dir folder)))]
    (cond
      (not (contains? settings :font))
      (do (say "The blog has the font of its theme already.") 0)

      (ask (str "Take away the font " (pr-str (:font settings))
                (when there? (str " and remove " folder)) "?"))
      (do (when there? (fs/delete-tree (fs/path dir folder)))
          (write-settings! dir (without-font (read-settings-text dir)))
          (say "The blog has the font of its theme. It is published with: bareblog publish --all")
          0)

      :else
      (do (say "Nothing was changed.") 1))))
