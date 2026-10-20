# Changelog

All notable changes to BareBlog are listed here. Until 1.0.0, a release may change the
settings, the layout of a blog and the commands.

## [0.1.0] - 2026-10-20

The first release. BareBlog needs BareDOM 4.11.0.

### Added

- **Write.** A post is a folder with `post.md`, written as Markdown, with its pictures and
  files beside it. A text holds only its text: the title, summary, tags and date are worked
  out from it. Pages that are no posts, such as About, live in `pages/`, and drafts in
  `drafts/`.
- **What a post can hold.** Headings, emphasis, links, footnotes, quotes, lists, photos,
  tables and code coloured by `x-code`. HTML in a fence marked `live` runs in a frame of its
  own, where BareDOM components work.
- **Preview.** `bareblog preview` shows the whole blog, drafts included, and every open page
  follows the files as they are saved. What is wrong with the blog is said in the terminal and
  above the page.
- **Publish.** `bareblog publish <folder>` and `bareblog publish --all` save every page as
  static HTML in `docs/`, drawn before any script runs, then commit and push. The pages serve
  from GitHub Pages or any web server.
- **Five blog templates:** Journal, Notebook, Magazine, Longform and Diary. Every BareDOM theme
  works with each, and a reader can pick one.
- **For readers:** a search, a choice of tag, an archive, an Atom feed, and a card for every
  page that a chat or a social site shows with a link.
- **The pictures of a blog:** a logo in place of the title and a favicon, each with a light and
  a dark version, and a picture for link previews.
- **Settings** in `blog.edn`, which `bareblog init` writes with every setting in it.
  `example/blog.edn` explains each one.
