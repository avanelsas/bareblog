# BareBlog

BareBlog is a small set of tools to write and publish a blog. Write a post as Markdown in a
folder. One command shows your blog in the browser. Another publishes it as static pages, on
GitHub Pages or on any web server: a home page, a page for each post and tag, an archive, your
own pages and an Atom feed. Each page is drawn before any script runs. When the script
arrives, a reader can search, pick a tag and pick a theme.

Another blog tool you ask? I wanted to start blogging again after a 20 year lapse, and while there are
excellent tools available already (e.g. [quickblog](https://github.com/borkdude/quickblog) by Michiel Borkent)
I also needed a project to test my other projects regarding web components. A blogging tool ticked all boxes for me.

BareBlog is built using [BareDOM](https://github.com/avanelsas/baredom) web components and the
BareMirror runtime that ships with it.

My own blog [Foggy Glasses](https://avanelsas.github.io/foggy-glasses/) is created using BareBlog.

## What you need

babashka, Java, Node 22 or later and git. To publish, also Chrome or Chromium.

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
path for that. Any other directory that is on it works as well.

## See what a post can hold

BareBlog comes with an example blog. Look at it before you write your own:

```
cd example
bareblog preview
```

Open the post **Every kind of block**. It has one of every kind of block that a post can hold.
Its text is [`example/posts/every-kind/post.md`](./example/posts/every-kind/post.md). Read it
beside the page to see how each one is written.

The example blog also has an About page, in `example/pages/`, and a draft, in `example/drafts/`.
To see it in another blog template, set `:template` to `:notebook`, `:magazine`, `:longform`
or `:diary` in `example/blog.edn`. The open pages follow.

## A blog, step by step

**1. Make a blog.**

```
mkdir my-blog && cd my-blog
bareblog init "My blog"
```

**2. Write a post, and look at it.**

```
bareblog new "A first post"
bareblog preview
```

This makes `drafts/a-first-post/post.md`. Write in it, and put photos beside it. The whole blog
opens in your browser, drafts included. Save a file and the page follows.

**3. Set `:author` and `:address` in `blog.edn`.** Both lines are in the file already, as
comments. Take the `;;` away and fill them in:

```clojure
 :author  "Your name"
 :address "https://<you>.github.io/<repository>/"
```

**4. Put the blog on GitHub, once.** Make an empty repository there, then:

```
git remote add origin git@github.com:<you>/<repository>.git
git add --all && git commit -m "A blog" && git push -u origin main
```

**5. Publish the post.**

```
bareblog publish a-first-post
```

The post moves from `drafts/` to `posts/`, the pages are written to `docs/`, and both are
committed and pushed.

**6. Turn GitHub Pages on, once.** On GitHub, under Settings and Pages, serve the branch
`main` from `/docs`. Do this after the first publish: before it there is no `docs/`, and the
build that GitHub starts fails.

## After that

| You want to | Do this |
|---|---|
| Change a published post | Edit it, then `bareblog publish <folder>` |
| Take a post offline | Move its folder back to `drafts/`, then `bareblog publish <folder>` |
| Change a setting, or a picture such as the logo | Edit `blog.edn` or replace the picture, then `bareblog publish --all` |
| Add a page that is no post, such as About | `bareblog new --page "About"`, write it, then `bareblog publish about` |
| See every tag, or write one tag one way | `bareblog tags`, then `bareblog tags rename baredom "BareDOM"` and `bareblog publish --all` |

Publishing one post reads only that post from disk. An unfinished edit in another post stays on
your machine.

## The blog templates

A blog template decides what the blog looks like. The posts stay the same: switch the template in
`blog.edn` and every page follows. Every theme works with every template.

| Template | What it is |
|---|---|
| `:journal` | One reading column. The home page lists the posts, each with its first picture small beside its summary |
| `:notebook` | A sidebar that lists every post by year, with the tags above it. The home page shows the newest post, and a command palette searches |
| `:magazine` | The home page shows the posts as cards in a grid. The newest post with a picture leads, on a card twice as wide and twice as high |
| `:longform` | The home page shows the posts as a deck of large cards, which pile up as the reader scrolls. A post opens on its first picture, with its title at the foot of it |
| `:diary` | A calendar beside the list of posts. Only the days with a post can be picked. A day or a month shows its posts |

## More

- [The guide](./GUIDE.md): what a blog is made of, what a post and a page hold, a logo and a
  favicon, link previews, and a blog on your own server.
- [Working on BareBlog](./CONTRIBUTING.md): how the code is laid out, and how to test it.

## License

[MIT](./LICENSE).
