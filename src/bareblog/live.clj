(ns bareblog.live
  "Tells the pages that are open when the files of the blog change.

   An edge beside `bareblog.serve`: nothing asks for this, and what it answers to is a file
   changing. The sockets, the thread and the clock are here. What a change means is
   `bareblog.watch`, which is pure."
  (:require [babashka.fs :as fs]
            [bareblog.layout :as layout]
            [bareblog.watch :as watch]
            [clojure.string :as str]
            [org.httpkit.server :as http]))

(def ^:private tick
  "How often the files are looked at, in milliseconds. Only their times are read."
  100)

(defonce ^{:private true :doc "The folder that the page behind each open socket shows."}
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
                        (filter fs/directory? (map (partial fs/path root) layout/post-dirs))))))

(defn- read-stamp
  "The stamp of the blog at `root`."
  [root]
  (into {} (map (partial stamp-entry root)) (list-files root)))

(defn- send! [text socket]
  (http/send! socket text))

(defn- tell!
  "Sends what `read-told` gives for `folder` to each of `sockets`."
  [read-told sockets folder]
  (run! (partial send! (pr-str (read-told folder))) sockets))

(defn- look!
  "Looks at the blog at `root` once, and tells the pages that the difference from the stamp
   `before` makes out of date. Returns the stamp it found."
  [root read-told before]
  (let [now       (read-stamp root)
        by-folder (watch/by-folder @listeners)]
    (doseq [folder (watch/to-tell (watch/changes (or before now) now) (set (keys by-folder)))]
      (tell! read-told (by-folder folder) folder))
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
   process runs. `read-told` gives what the page of a folder is told to show, and does not
   throw."
  [root read-told]
  (future
    (loop [state {}]
      (Thread/sleep tick)
      (recur (looked! root read-told state)))))

(defn socket
  "The socket of the page of `folder`. The page is told what to show when the socket opens,
   and again whenever its files change. The page says nothing back."
  [read-told request folder]
  (http/as-channel request
                   {:on-open  (fn [channel]
                                (swap! listeners assoc channel folder)
                                (tell! read-told [channel] folder))
                    :on-close (fn [channel _status]
                                (swap! listeners dissoc channel))}))
