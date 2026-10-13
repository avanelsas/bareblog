(ns bareblog.preview.live
  "Tells the pages that are open when the files of the blog change.

   An edge beside `bareblog.preview.serve`: nothing asks for this, and what it answers to is a
   file changing. The sockets, the thread and the clock are here. What a change means is
   `bareblog.preview.watch`, which is pure."
  (:require [babashka.fs :as fs]
            [bareblog.blog.layout :as layout]
            [bareblog.preview.watch :as watch]
            [clojure.string :as str]
            [org.httpkit.server :as http]))

(def ^:private tick
  "How often the files are looked at, in milliseconds. Only their times are read."
  100)

(defonce ^{:private true :doc "The page behind each open socket."}
  listeners
  (atom {}))

(defn- stamp-entry [root path]
  [(str/join "/" (map str (fs/relativize root path)))
   (fs/file-time->millis (fs/last-modified-time path))])

(defn- list-files
  "The files of the blog at `root` that a page is made from."
  [root]
  (filter fs/regular-file?
          (cons (fs/path root layout/settings-file)
                (mapcat #(fs/glob % "**")
                        (filter fs/directory? (map (partial fs/path root) layout/written-dirs))))))

(defn- read-stamp
  "The stamp of the blog at `root`."
  [root]
  (into {} (map (partial stamp-entry root)) (list-files root)))

(defn- send! [text socket]
  (http/send! socket text))

(defn- tell!
  "Sends each page of `by-page` what `read-told` gives for it, over each of its sockets.
   `read-told` takes all the pages at once, so it reads what they share once."
  [read-told by-page]
  (let [told (read-told (keys by-page))]
    (doseq [[page sockets] by-page]
      (run! (partial send! (pr-str (told page))) sockets))))

(defn- look!
  "Looks at the blog at `root` once, and tells every open page when the stamp differs from
   `before`. Returns the stamp it found."
  [root read-told before]
  (let [now (read-stamp root)]
    (when (and before (watch/changed? before now))
      (tell! read-told (watch/by-page @listeners)))
    now))

(defn- say-once!
  "Prints `problem` unless it is what was `said` last. Returns what is now said last."
  [said ^Throwable problem]
  (let [saying (str "Could not look at the blog: " (.getMessage problem))]
    (when (not= said saying)
      (println saying))
    saying))

;; A look that fails keeps its stamp, so the next look tries the same difference again. That
;; is right for what can fail here, a file that went while it was listed. What a file holds
;; is read by `read-told`, which does not throw.
(defn- looked!
  "The stamp and what was said last, after one look."
  [root read-told {:keys [stamp said]}]
  (try
    {:stamp (look! root read-told stamp)}
    (catch Throwable problem
      {:stamp stamp :said (say-once! said problem)})))

(defn watch!
  "Looks at the blog at `root` again and again, on a thread of its own, for as long as the
   process runs. `read-told` gives what each of some pages is told to show, by page, and does
   not throw."
  [root read-told]
  (future
    (loop [state {}]
      (Thread/sleep tick)
      (recur (looked! root read-told state)))))

(defn socket
  "The socket of the page `page`. The page is told what to show when the socket opens, and
   again whenever a file of the blog changes. The page says nothing back."
  [read-told request page]
  (http/as-channel request
                   {:on-open  (fn [channel]
                                (swap! listeners assoc channel page)
                                (tell! read-told {page [channel]}))
                    :on-close (fn [channel _status]
                                (swap! listeners dissoc channel))}))
