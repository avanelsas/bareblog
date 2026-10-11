# BareBlog

**A blog publishing tool built on BareDOM components and the BareMirror runtime.**

**Part of a set.** [BareDOM](https://github.com/avanelsas/baredom) is the web component library.
[BareMirror](https://github.com/avanelsas/baredom/blob/main/docs/baremirror.md) ships inside it and
brings a page to what an application wants to show. BareBlog uses both to publish a blog as static
pages.

## State

Four of ten steps are built: one post, read from its Markdown, shows on a page with every kind of
block, and the open page follows the file as you save it. There is no `bareblog` command yet, and nothing is published. This page describes what BareBlog will do.

To see the example blog:

```
npm install
npx shadow-cljs watch app
bb serve example
```

## What it will do

You write a post as Markdown in a folder. One command shows the whole blog in your browser. Another
publishes the post to GitHub Pages.

```
my-blog/
  drafts/                   on your disk only
    a-new-idea/
      post.md
  posts/
    a-week-in-lisbon/
      post.md
      harbour.jpg
```

- **A folder is a post.** Its name is the address of the post, and its photos sit beside the text.
- **A draft stays local.** Publishing a post moves it out of `drafts/`.
- **One command covers new, edited and removed posts.** Publish takes a folder name and works out
  which of the three it is.
- **The author writes content only.** A blog template decides the look, and a theme decides colour
  and type.

## What you get

A set of static pages: a home page, a page per post, tag pages, an archive and a feed. Each page is
drawn before any script runs. When the script arrives, search, the tag filter and the theme switch
come alive.

Two blog templates to start:

- **Journal**: one reading column, with the posts in numbered pages.
- **Notebook**: a sidebar of posts by year, with search in a command palette.

## How BareMirror is used

One set of templates serves three moments.

| Moment | What happens |
|---|---|
| Generating | Each page is rendered once in a headless browser and saved. |
| Preview | A saved Markdown file updates the open page in place. |
| A reader's visit | The saved page is adopted, and the live parts run on it. |

## Who it is for

ClojureScript developers. BareBlog assumes Java, Node and shadow-cljs.

## License

[MIT](./LICENSE).
