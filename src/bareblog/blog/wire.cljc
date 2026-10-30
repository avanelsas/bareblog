(ns bareblog.blog.wire
  "The names that the command and the app share.

   A page loads two files. The data of the site, at the top of the blog, is the same for every
   page. The data of a page, beside the page, says what the page is, and holds under `:text`
   the post or the page of the author that it shows, when it shows one. In a preview it names a
   socket, and the page is told both over that."
  (:require [clojure.string :as str]))

(def site-data
  "The file at the top of a blog that holds what every page loads: the settings of the blog and
   its list of posts."
  "site.edn")

(def page-data
  "The file that holds the data of a page, beside the page."
  "page.edn")

(def changes
  "The address, beside a page, of the socket that tells the page what to show when its files
   change. The data of a page names it when there is such a socket."
  "changes")

(def index
  "The name of the file that holds the page of a directory."
  "index.html")

(def reserved
  "The names beside a page that are not the name of a file of its post."
  #{page-data changes index})

(def feed
  "The file at the top of a blog that holds its feed."
  "feed.xml")

(def font
  "The file at the top of a blog that holds the stylesheet of its font, which the page of the
   app names in its head. It is empty when the settings name no font."
  "font.css")

(def feed-type
  "The media type of the feed, which is Atom. The page of the app names it too, in its head."
  "application/atom+xml")

(def missing
  "The file at the top of a blog that a host sends for an address with no page."
  "404.html")

(def page-colours
  "The names under which the browser keeps the colour of the page, in light and in dark mode.
   The page of the app reads them in its head, before anything is drawn."
  {:light "bareblog-colour-light" :dark "bareblog-colour-dark"})

(def root-attribute
  "The attribute of the document of a page that holds where the top of the blog is."
  "data-root")

(def missing-attribute
  "The attribute of the document of a page that says the page is the one for an address with
   no page."
  "data-not-found")

(def comment-limits
  "What a comment that a reader sends may be: the most characters of a name and of a text, how
   long its form is open at least, in milliseconds, and how many comments may wait for the
   author. The form and the script take them from here."
  {:name-max    80
   :text-max    4000
   :open-min-ms 5000
   :waiting-max 100})

(def refusals
  "Each refusal of the script to a reader, by the start of its error, as its kind. The page and
   the command read an error by these."
  {"comments are closed" :closed
   "the inbox is full"   :full
   "a name of"           :name
   "a comment of"        :text})

(defn- refused-as
  "The kind of the refusal that starts with `start`, when the error `error` is that refusal."
  [error [start kind]]
  (when (str/starts-with? (str error) start) kind))

(defn refusal
  "The kind of the refusal of the script that the error `error` is, or nil."
  [error]
  (some (partial refused-as error) refusals))

(defn comment-body
  "What the script of comments is sent for a comment: the `:post` it is under, the `:name` and
   the `:text`, the comment it answers as `:reply-to`, the hidden field as `:trap`, and how long
   the form was `:open`, in milliseconds. The script reads it as JSON."
  [{commenter :name :keys [post text reply-to trap open]}]
  {:post    (or post "")
   :name    commenter
   :text    text
   :replyTo (or reply-to "")
   :trap    (or trap "")
   :open    open})
