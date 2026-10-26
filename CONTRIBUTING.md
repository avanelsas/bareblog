# Working on BareBlog

```
bb test
npx shadow-cljs watch app
cd example && ../bin/bareblog preview
```

The code is in `src/bareblog/`, with a folder for each thing BareBlog does. A folder uses the
ones above it in this list, and none below it:

| Folder | What is in it |
|---|---|
| `blog/` | What a blog is: a post, the list of posts, the address of each page. The command and the browser app share this folder and nothing else |
| `app/` | The app that runs in the browser: the five blog templates, the view and the rendering, on BareMirror |
| `written/` | What a blog is made of once it is written out: its pages, its files and its feed |
| `read/` | From a directory to data: Markdown to blocks, and a blog from its folders |
| `preview/` | The local server, and what tells an open page that a file changed |
| `publish/` | From data to pages on GitHub: the plan of a publish, the saved pages, git |
| `command/` | The `bareblog` command, what `init` and `new` make, the tags and the series of a blog, and BareBlog brought to its latest release |

The tests are in `test/bareblog/`, in the same folders. A test holds this table to the code.
`clj-kondo --lint src test` is clean before a commit.

A change comes in as a pull request from a branch, and CI runs on it. How a version is
released is in [RELEASING.md](RELEASING.md).
