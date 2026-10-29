(ns bareblog.blog.wire
  "The names that the command and the app share.

   A page loads two files. The data of the site, at the top of the blog, is the same for every
   page. The data of a page, beside the page, says what the page is, and holds under `:text`
   the post or the page of the author that it shows, when it shows one. In a preview it names a socket, and the page is told both over that.")

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
  "What a comment that a reader sends may be: the most characters of a name and of a text, and
   how long its form is open at least, in milliseconds. The form and the script take them from
   here."
  {:name-max    80
   :text-max    4000
   :open-min-ms 5000})
