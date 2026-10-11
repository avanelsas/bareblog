(ns bareblog.layout
  "Where the things of a blog are in its directory.")

(def settings-file "blog.edn")

(def post-file "post.md")

(def post-dirs
  "The directories that hold a folder for each post: the published ones, and the drafts."
  ["posts" "drafts"])
