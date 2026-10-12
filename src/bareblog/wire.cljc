(ns bareblog.wire
  "The names that the command and the app share.")

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

(def missing
  "The file at the top of a blog that a host sends for an address with no page."
  "404.html")

(def root-attribute
  "The attribute of the document of a page that holds where the top of the blog is."
  "data-root")

(def missing-attribute
  "The attribute of the document of a page that says the page is the one for an address with
   no page."
  "data-not-found")
