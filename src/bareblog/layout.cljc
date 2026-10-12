(ns bareblog.layout
  "Where the things of a blog are in its directory.")

(def settings-file "blog.edn")

(def post-file "post.md")

(def posts-dir "posts")

(def drafts-dir "drafts")

(def output-dir
  "The directory that holds the published blog: its pages, and the data that they load."
  "docs")

(def post-dirs
  "The directories that hold a folder for each post: the published ones, and the drafts."
  [posts-dir drafts-dir])
