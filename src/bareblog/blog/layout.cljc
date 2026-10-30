(ns bareblog.blog.layout
  "Where the things of a blog are in its directory.")

(def settings-file "blog.edn")

(def readme-file "README.md")

(def series-file
  "The file beside the settings that holds the series of the blog, which only `bareblog series`
   writes."
  "series.edn")

(def fonts-dir
  "The directory beside the settings that holds a folder for the font of the blog, which
   `bareblog font` writes. A published blog holds it at its top."
  "fonts")

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

(def comments-file
  "The file beside a post that holds its published comments, which only `bareblog comments`
   writes."
  "comments.edn")
