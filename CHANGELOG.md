# Changelog

All notable changes to BareBlog are listed here. Until 1.0.0, a release may change the
settings, the layout of a blog and the commands.

## [Unreleased]

### Added

- **`bareblog update`** shows what changed since the installed version, asks, and brings
  BareBlog to its latest release. A blog shows the new version once it is published again.
  `bareblog version` says which version is installed.
- **A README for a new blog.** `bareblog init` writes a `README.md` with where things go and
  the commands that write and publish the blog. A `README.md` that is there already is kept.

## [0.3.0] - 2026-10-24

BareBlog needs BareDOM 4.15.0.

### Added

- **A cover for a post.** A photo on a line of its own that opens a post, right after its
  title, is its cover. Each blog template shows it in its own place: under the title in
  Journal, Notebook and Diary, above it in Magazine, and behind it in Longform. The text does
  not show it again. A list and a link preview still show the first picture of a post.

### Changed

- **The text of a post is 750 pixels wide,** and the column of every page is wider, so the
  headings of a post still stand beside its text. The lists keep their width.
- **In Longform a post opens on its cover.** A post whose pictures come later in its text opens
  on its title alone.
- **A page of the author opens with its article,** as a post does, with no blurred backdrop of
  its own. A blog that wants colours behind it uses `:page-colors`.

### Fixed

- **In Diary the page numbers stand under the list,** and not under the calendar.

## [0.2.0] - 2026-10-23

BareBlog needs BareDOM 4.15.0.

### Added

- **How far a post is read.** A line along the top of the window fills as the reader scrolls,
  in the colours of the theme. A post that fits in the window has none.
- **The headings of a post beside its text,** headed Content. The list marks the heading the
  reader is at, stays in view while the page scrolls, and goes to a heading on a click. It
  shows in every blog template where the column holds both.
- **`bareblog tags`** lists every tag with how many posts and pages have it.
  `bareblog tags rename old "new"` writes a tag anew in every post, draft and page, after it
  shows what changes and asks.
- **Three settings, each off unless true:**
  - `:gradient-titles` fills the titles of posts and pages, also in a list, with a gradient of
    the colours of the theme. The title of a post moves its gradient once as the page opens.
  - `:navbar-colors` lets the colours of the theme drift behind the navbar.
  - `:page-colors` lays them still behind the whole page.

### Changed

- **A tag written in more than one way stops a publish.** Tags that differ only in case, or in
  a dash, an underscore or a space, are one tag. The preview shows the problem, with the
  `bareblog tags rename` that writes them one way.
- **Titles grow with the window,** a post's to 4rem and those in a list to 2.25rem.
- **Pulling past the end of a page** shows the colour of the page, and only a narrow window can
  be pulled.
- **In Notebook the navbar sticks** to the top of the window.
- **Releases** are made by a pull request and a tag. See `RELEASING.md`.

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
