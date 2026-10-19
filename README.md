# BareBlog

BareBlog is a small set of tools to write and publish a blog. Write a post as Markdown in a
folder. One command shows your blog in the browser. Another publishes it as static pages, on
GitHub Pages or on any web server.

BareBlog is built using [BareDOM](https://github.com/avanelsas/baredom) web components and the
BareMirror runtime that ships with it.

## What you need

babashka, Java, Node 22 or later, Chrome and git.

## Set up, once

```
git clone https://github.com/avanelsas/bareblog
cd bareblog
npm install
npx shadow-cljs release app
mkdir -p ~/.local/bin
ln -s "$PWD/bin/bareblog" ~/.local/bin/bareblog
```

The last two lines put the `bareblog` command on your path. `~/.local/bin` has to be on your
path for that; any other directory that is on it will do as well.

## See what a post can hold

BareBlog comes with an example blog. Look at it before you write your own:

```
cd example
bareblog preview
```

Open the post **Every kind of block**. It has one of everything a post can hold: headings,
strong, emphasised and struck text, inline code, links, footnotes, a quote, lists with and
without numbers, a photo, an image in a line, code in JavaScript and in Clojure, a divider and
a table. It also shows that HTML typed into a post is shown as code, not run.

Its text is [`example/posts/every-kind/post.md`](./example/posts/every-kind/post.md). Read it
beside the page to see how each of these is written.

The example blog also has an About page, in `example/pages/`, and a draft, in `example/drafts/`.
To see it in another blog template, set `:template` to `:notebook`, `:magazine`, `:longform`
or `:diary` in `example/blog.edn`. The open pages follow.

## A blog, step by step

**1. Make a blog.**

```
mkdir my-blog && cd my-blog
bareblog init "My blog"
```

**2. Write a post.**

```
bareblog new "A first post"
```

This makes `drafts/a-first-post/post.md`. Write in it. Put photos beside it. A file beside a
post is published when the post shows it or links to it, and stays on your disk when it does
not. The preview tells you when a post names a file that is not there.

**3. Look at it.**

```
bareblog preview
```

The whole blog opens in your browser, drafts included. Save a file and the page follows.

**4. Set `:author` and `:address` in `blog.edn`.** Do this before you publish. Both lines are
in the file already, as comments. Take the `;;` away and fill them in:

```clojure
 :author  "Your name"
 :address "https://<you>.github.io/<repository>/"
```

Every link of a published page starts with the address. A blog on GitHub Pages lives under the
name of its repository, so one that is published without its address has links that lead
nowhere.

**5. Put the blog on GitHub, once.** Make an empty repository there, then:

```
git remote add origin git@github.com:<you>/<repository>.git
git add --all && git commit -m "A blog" && git push -u origin main
```

**6. Publish the post.**

```
bareblog publish a-first-post
```

The post moves from `drafts/` to `posts/`, the pages are written to `docs/`, and both are
committed and pushed.

**7. Turn GitHub Pages on, once.** On GitHub, under Settings and Pages, serve the branch
`main` from `/docs`. Do this after the first publish: before it there is no `docs/`, and the
build that GitHub starts fails.

GitHub does not link to the blog from the page of the repository by itself. To add the link,
click the gear beside "About" on that page and tick "Use your GitHub Pages website".

## After that

| You want to | Do this |
|---|---|
| Change a published post | Edit it, then `bareblog publish <folder>` |
| Take a post offline | Move its folder back to `drafts/`, then `bareblog publish <folder>` |
| Change the title, the template or the theme | Edit `blog.edn`, then `bareblog publish --all` |
| Change the link preview picture | Replace the file beside `blog.edn`, then `bareblog publish --all` |
| Leave out the line "Powered by BareBlog" | Set `:powered-by-footer false` in `blog.edn`, then `bareblog publish --all` |
| Add a page that is no post, such as About | `bareblog new --page "About"`, write it, then `bareblog publish about` |

Publishing one post reads only that post from disk. An unfinished edit in another post stays on
your machine.

## What a blog is

```
my-blog/
  blog.edn              the settings
  .gitignore            what git leaves alone: the drafts, and the files a Mac adds
  drafts/               on your disk only
    a-new-idea/
      post.md
  posts/                published
    a-first-post/
      post.md
      harbour.jpg
  pages/                pages that are no posts
    about/
      page.md
  docs/                 the pages, written by bareblog
```

A folder is a post, and its name is the address of the post. You write content only: a blog
template decides the look.

## A post

A post is only its text. There is no block of settings above it.

```
# A week in Lisbon

We arrived late on Sunday and walked straight down to the river.

![The harbour at dawn](harbour.jpg)

#travel #portugal #slow_trains
```

BareBlog works out the rest:

| What | Where it comes from |
|---|---|
| The title | The first heading, when the post starts with one. Without it, the name of the folder as words: `a-first-post` is "A first post" |
| The summary | The start of the first paragraph |
| The tags | The last line, when it holds nothing but tags |
| The date | The day you first publish the post |
| The picture in lists and link previews | The first picture of the post. A link preview takes the first PNG, JPEG or GIF |

**Tags.** Write them on the last line, each with a `#` and with no space in it:

| You write | The tag is |
|---|---|
| `#travel` | travel |
| `#two_words` | two words |
| `#x-code` | x-code |

An underscore stands for a space, so a tag of two words is written as `#two_words`. A dash
stays a dash. A tag does not start with an underscore. The line of tags is not shown in the
post. A `#word` anywhere else in the text is just text.

A last line that holds nothing but words with a `#` is always read as tags. A post that ends
with a line such as `#1` gets the tag "1", and does not show the line.

**The date** cannot be written. A post that was never published has none. In the preview it
shows without a date, at the top of the list, which is where it is once you publish it. Posts
that are published on one day stand in the order they were published, the last one first.

## A page

A folder in `pages/` is a page of its own, such as About. It has a link at the top of every
page of the blog, and is in none of the lists of posts.

```
# About

Who writes this blog.
```

A page is only its text too, and its title is its first heading. The links stand in the order
the pages were first published: the page you publish first comes first. Pages that are
published together, as with `bareblog publish --all`, stand by title. A page is written
where it is published from, so it has no draft. The preview marks it as not published until
you publish it by its folder.

## The blog templates

A blog template decides how the blog looks. The posts stay the same: switch the template in
`blog.edn` and every page follows. Every theme works with every template.

| Template | What it is |
|---|---|
| `:journal` | One reading column. The home page lists the posts, each with its first picture small beside its summary |
| `:notebook` | A sidebar that lists every post by year, with the tags above it. The home page shows the newest post, and a command palette searches |
| `:magazine` | The home page shows the posts as cards in a grid. The newest post with a picture leads, on a card twice as wide and twice as high |
| `:longform` | The home page shows the posts as a deck of large cards, which pile up as the reader scrolls. A post opens on its first picture, with its title at the foot of it |
| `:diary` | A calendar beside the list of posts. Only the days with a post can be picked. A day or a month shows its posts |

## Link previews

A chat or a social site that shows a link to your blog reads a card in the page: its title, its
summary and a picture. BareBlog writes that card in every page.

- A post or a page shows its own title and summary, and its first PNG, JPEG or GIF. A chat
  shows no SVG, and some show no WebP.
- Any other page, such as the home page, shows the name of the blog.
- A page with no picture of its own shows the link preview picture: a PNG, JPEG or GIF beside
  `blog.edn`, which `:link-preview` names. A square one of about 400 by 400 is shown small
  beside the text, a wide one of 1200 by 630 beneath it.

A card needs full addresses, so it has a picture and a link only when `:address` is set.
Publishing one post keeps the link preview picture as it was published. Publish a new one with
`bareblog publish --all`.

A chat keeps a card it has shown. To see a new one, share the address again with something
after it, such as `?v=2`.

## The settings

`bareblog init` writes `blog.edn` with every setting in it. Those that have a value when you
say nothing are written with that value, so the file shows what your blog does and what you
can change. `:author` and `:address` are written as comments, since only you know them. **Set
both before you publish.** `:link-preview` is a comment too, since it names a picture that you
add.

```clojure
{:title            "My blog"
 :language         "en"
 :template         :journal
 :available-themes ["ocean" "default" "forest" "sunset" "neo-brutalist" "aurora" "mono-ai" "warm-mineral"]
 :per-page         10

 ;; The line "Powered by BareBlog" at the bottom of every page. false leaves it out.
 :powered-by-footer true

 ;; Set these two before you publish.

 ;; Who writes the blog, in its feed. The title of the blog when left out.
 ;; :author "Your name"

 ;; Where the blog is published. Every link of a published page starts there.
 ;; :address "https://<you>.github.io/<repository>/"

 ;; The picture that a chat shows with a link to a page that has none of its own: a
 ;; PNG, JPEG or GIF beside this file. A square one of 400x400 is shown small beside
 ;; the text, a wide one of 1200x630 beneath it.
 ;; :link-preview "link-preview.jpg"
 }
```

| Key | What it is | When left out |
|---|---|---|
| `:title` | The name of the blog | |
| `:language` | The language of the pages | `"en"` |
| `:template` | `:journal`, one reading column, `:notebook`, a sidebar of posts and a command palette, `:magazine`, the posts as cards in a grid, `:longform`, a deck of large cards, or `:diary`, a calendar beside the posts | `:journal` |
| `:available-themes` | The themes a reader can pick from, by the name of a BareDOM theme preset. The first is the one a reader starts with. A new blog has all of them: take out the ones you do not want | BareDOM's own look, and nothing to pick |
| `:per-page` | How many posts a page of the home page shows | `10` |
| `:powered-by-footer` | Whether every page ends with the line "Powered by BareBlog". Only `false` leaves it out | `true` |
| `:author` | The author in the feed | The title |
| `:address` | Where the blog is published, as a full URL | The top of the host, which is wrong for a blog on GitHub Pages under a repository |
| `:link-preview` | The picture that a chat shows with a link to a page that has none of its own: a PNG, JPEG or GIF beside `blog.edn` | No picture |

## What a reader gets

Static pages: a home page, a page for each post and tag, an archive, your own pages and an Atom
feed, which every page links to as "Feed" and which a feed reader takes like an RSS feed. Each
page is drawn before any script runs. When the script arrives, a reader can search, pick a tag
and pick a theme.

A list of posts shows the first picture of each post. A link to any page shows a card in a chat.
Every page ends with the line "Powered by BareBlog", unless the settings leave it out.

## On your own server

`docs/` holds static files only, so any web server can serve it. Set `:address` to where the
blog lives, such as `https://blog.example.com/`. A blog under a path, such as
`https://example.com/notes/`, works too.

- **The page for an address with no page** is `404.html`. GitHub Pages shows it by itself.
  Another server is told: `error_page 404 /404.html;` in nginx, `ErrorDocument 404 /404.html`
  in Apache.
- **`.nojekyll` and `CNAME`** are for GitHub Pages. Another server ignores them.
- **Getting the files there.** `bareblog publish` commits and then pushes to `origin`. Make
  `origin` a repository on your server, with a hook that copies `docs/` to the web root, and a
  publish is one step. Without a remote, the publish writes and commits the pages, and says
  that git could not push. Copy `docs/` yourself then, for example with
  `rsync -a --delete docs/ you@server:/var/www/blog/`.

## Working on BareBlog

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
| `command/` | The `bareblog` command, and what `init` and `new` make |

The tests are in `test/bareblog/`, in the same folders. [`CLAUDE.md`](./CLAUDE.md) lists every
file with what it is, and the rules the code keeps.

## License

[MIT](./LICENSE).
