(ns bareblog.blog.layout
  "Where the things of a blog are in its directory.")

(def settings-file "blog.edn")

(def posts-dir "posts")

(def drafts-dir "drafts")

(def pages-dir
  "The directory that holds a folder for each page of the blog that is no post, such as one
   about its author."
  "pages")

(def output-dir
  "The directory that holds the published blog: its pages, and the data that they load."
  "docs")

(def written-dirs
  "The directories that an author writes in. A post goes before a page of the same name."
  [posts-dir drafts-dir pages-dir])

(def text-file
  "The name of the file that holds the text, in a folder of each directory."
  {posts-dir  "post.md"
   drafts-dir "post.md"
   pages-dir  "page.md"})

(def text-files
  "The names that the file with the text of a folder can have."
  (set (vals text-file)))
