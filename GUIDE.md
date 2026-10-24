# The BareBlog guide

What the [README](./README.md) leaves out: what a blog is made of, what a post and a page hold,
the pictures of the blog, link previews, and a blog on your own server.

## What a blog is

```
my-blog/
  blog.edn              the settings
  logo.svg              the pictures of the blog, if you want them: its logo,
  logo-dark.svg           its favicon and its link preview, each where the
  favicon.svg             settings name it
  link-preview.jpg
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
      portrait.jpg
  docs/                 the pages, written by bareblog
```

A folder is a post, and its name is the address of the post. A file beside a post is published
when the post shows it or links to it, and stays on your disk when it does not. The preview
tells you when a post names a file that is not there.

`blog.edn` holds the settings: `bareblog init` writes every one of them, and
[`example/blog.edn`](./example/blog.edn) explains each in a comment.

`docs/` belongs to BareBlog. Every publish writes it again, so do not change it by hand.

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
| The tags | The last line, as `#travel #two_words`. An underscore is a space |
| The date | The day you first publish the post. It cannot be written |
| The cover | A picture on a line of its own that opens the post, right after the title. Each blog template shows it in its own way, and not again in the text |
| The picture in lists and link previews | The first picture of the post, which is its cover when it has one. A link preview takes the first PNG, JPEG or GIF |

The line of tags is not shown, and a `#word` anywhere else is just text.

A tag is one tag in any case, and with a dash, an underscore or a space between its words:
`#BareDOM`, `#baredom` and `#bare_dom` are the same tag. Write it one way in every post. A
publish stops while a tag is written in more than one way, and the preview says which.

`bareblog tags` lists every tag, with how many posts and pages have it, and each tag that is
written in more than one way. `bareblog tags rename baredom BareDOM` writes a tag anew in every
post, draft and page. It shows what changes and asks first. Publish the posts it changed with
`bareblog publish --all`.

What a post can hold, from headings to live HTML, is shown in the example post
[`every-kind/post.md`](./example/posts/every-kind/post.md).

## A page

A folder in `pages/` is a page of its own, such as About. It has a link at the top of every
page of the blog, and is in none of the lists of posts.

A page is only its text too, with its first heading as its title. The links stand in the
order the pages were first published, and pages published together stand by title. A page
has no draft: the preview marks it as not published until you publish it.

## The pictures of the blog

The settings can name three pictures beside `blog.edn`:

| Setting | What it is | Formats |
|---|---|---|
| `:logo` | The brand of the navbar, in place of the title. It can hold the name of the blog in a font of its own | SVG, PNG, JPEG, GIF, WebP |
| `:favicon` | The icon of the blog in the tab of a browser | SVG, PNG, ICO |
| `:link-preview` | The picture of a link to a page that has none of its own | PNG, JPEG, GIF |

A logo and a favicon can have a picture for each mode of the theme:
`{:light "logo.svg" :dark "logo-dark.svg"}`. The one under `:dark` is shown when the theme is
dark. One name, as `"logo.svg"`, is the picture in both modes. A browser keeps a picture until
it changes, and then takes the new one.

## Link previews

A chat that shows a link to your blog, such as Slack, reads a card in the page: a title, a
summary and a picture. BareBlog writes that card in every page. A page shows its first PNG,
JPEG or GIF, or else the picture that `:link-preview` names.

A card needs full addresses, so it has a picture and a link only when `:address` is set. A chat
keeps a card it has shown. To see a new one, share the address again with something after it,
such as `?v=2`.

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
