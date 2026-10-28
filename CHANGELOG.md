# Changelog

All notable changes to BareBlog are listed here. Until 1.0.0, a release may change the
settings, the layout of a blog and the commands.

## [0.7.0] - 2026-10-28

BareBlog needs BareDOM 4.16.0.

### Added

- **A texture over the background.** `:texture :grain` lays a fine noise, as of film, over the
  background of every page, and `:texture :paper` soft blotches, thin fibres and a fine tooth.
  It lies under the text, takes the colours of the theme in light and in dark, and is not
  printed. A page with a texture holds its links and muted text a little darker on a light
  page and a little lighter on a dark one, so that every theme keeps WCAG AA contrast over it.
  `bareblog init` writes `:texture :none`.

## [0.6.2] - 2026-10-27

BareBlog bumps the BareDOM version to 4.16.0 to improve digital accessibility by following
WCAG AA contrast guidelines in every theme.

## [0.6.1] - 2026-10-27

BareBlog needs BareDOM 4.15.0.

### Changed

- **No word in a post is broken with a hyphen.** The running text of a post and its lists
  wraps between words. A word too long for the column, as a long address, breaks where it
  must rather than run past it.

## [0.6.0] - 2026-10-26

BareBlog needs BareDOM 4.15.0.

### Added

- **The Essay blog template.** A post opens on its cover across the window, with its title
  over the foot of it. Its contents are in the left margin, and its notes stand beside their
  lines in the right one, where a landscape photo reaches too. In a narrower window the
  contents are above the text, and in a narrow one the notes are footnotes. A list shows each
  post under its cover. Set it with `:template :essay`.
- **A font of the blog's own.** `bareblog font "Name"` fetches a family from Google Fonts into
  `fonts/`, with its licence, and sets `:font` in `blog.edn`. Every reader sees it in the
  running text of posts and lists. `bareblog font` says which font the blog has, and
  `bareblog font --none` goes back to the theme's font. A `:font` with no folder is shown to a
  reader who has it installed.
- **A block rises into place.** A photo, code, a table, a quote or a live block rises once as
  it enters the window, in a browser that can start an animation on scroll, and not for a
  reader who asks for less motion.

### Changed

- **Lines are shorter.** The text of a post is 624 pixels wide, about 70 letters to a line.
- **Links and muted text keep their contrast** against the background of every theme.
- **Text in dark mode is less bright,** with a little more room between its lines and letters.
- **A photo narrower than a landscape is shown at its own size,** never enlarged, never taller
  than most of the window, and in the middle of the text. A landscape photo is as wide as
  before.

## [0.5.0] - 2026-10-26

BareBlog needs BareDOM 4.15.0.

### Added

- **A series of posts.** `bareblog series --name "A series" "A post" "Another post"` makes a
  series of these posts in this order, by title or folder, after it shows what changes and
  asks. Naming it again with every part replaces it, and naming it with no posts removes it,
  while the posts stay. `bareblog series` lists every series. A series is kept in
  `series.edn` and goes live with the next publish of any post. At the foot of each part, a
  card shows the series beside the other posts, folds over every part, and links to the next
  one. A post is in one series at most, and a draft part joins once it is published. A series
  that names a missing post stops a publish, with the command that mends it.
- **A change of page moves.** When a reader opens a post from a list, the picture of its card
  grows into its cover, and its title moves into place. It shows on a published blog in
  Chrome, Edge and Safari, and not for a reader who asks for less motion.

### Fixed

- **A page starts in the colour of its theme,** and not white, before its post is shown. The
  preview showed white between pages.

## [0.4.0] - 2026-10-24

BareBlog needs BareDOM 4.15.0. BareBlog 0.3.0 and before have no `bareblog update`: update to
0.4.0 once by hand, in the directory of BareBlog, with `git fetch --tags`, `git checkout v0.4.0`,
`npm ci` and `npx shadow-cljs release app`.

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
