(ns bareblog.post
  "A post, and the name of its folder.")

(def folder-pattern
  "What the name of a post folder looks like, as the text of a pattern. The name is the address
   of the post."
  "[a-z0-9][a-z0-9-]*")

(defn from
  "The post in the folder `folder`, from what the reader read of its text. Its title is the one
   in its header block, or else the heading that opens it, or else the name of its folder."
  [folder {:keys [fields heading blocks]}]
  (assoc fields
         :title  (or (:title fields) heading folder)
         :blocks blocks))
