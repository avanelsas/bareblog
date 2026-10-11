(ns bareblog.wire
  "The names that the command and the app share.")

(def page-data
  "The file that holds the data of a page, beside the page."
  "page.edn")

(def changes
  "The address, beside a page, of the socket that tells the page what to show when its files
   change. The data of a page names it when there is such a socket."
  "changes")

(def reserved
  "The names beside a page that are not the name of a file of its post."
  #{page-data changes})
