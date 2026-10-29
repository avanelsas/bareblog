# Comments

A reader can write a comment under a post. The comment goes to a Google Sheet of your own, and
waits there. On your machine, `bareblog comments` shows you each one, and what you approve is
published with the post. Nothing reaches the blog without your yes.

Comments are off until you turn them on, and they are for posts only, not for pages.

## Before you start

- A blog that is published, as the [README](./README.md) says, in steps 1 to 6.
- A Google account.

## Set up, once

**1. Connect.** In the directory of your blog:

```
bareblog comments connect
```

The template comments sheet of BareBlog opens in your browser, on a page that offers a copy of
it. The sheet holds a small script, which adds a reader's comment to the sheet, and lets
`bareblog comments` read and delete them.

**2. Make a copy.** Click Make a copy. The copy is a sheet of your own, with the script in it.

**3. Deploy the script.** In the copy: Extensions, Apps Script. Then Deploy, New deployment.
Pick the type Web app, with:

- Execute as: Me
- Who has access: Anyone

Click Deploy. Google asks you to authorize access to your data: pick your account, and Allow.

**4. Paste the address.** The deploy shows the address of the web app, ending in `/exec`. Copy
it, and paste it in the terminal, where `connect` waits for it.

`connect` then asks to turn comments on. Say yes, and every page is published with the form
under each post.

## Read and approve

```
bareblog comments
```

It reads the sheet and shows each comment that waits, oldest first:

```
1 of 2 · a-week-in-lisbon · Ann · 3 hours ago
In answer to you: It was. By nine the queue went round the corner.
Was it worth the early start?
[a]pprove, [d]ecline, [r]eply, [s]kip or s[t]op?
```

| You type | What happens |
|---|---|
| `a` | The comment is approved |
| `d` | You are asked to confirm, and then its row is deleted. This cannot be undone |
| `r` | You write a reply on one line, or press Enter to write it in your editor. The comment is approved, and your reply goes under it |
| `s` | The comment is skipped. It comes again the next time |
| `t` | The walk-through stops |

After the last comment, or a stop, you see what you decided, and the approved comments and your
replies are published in one commit. Their rows are deleted once that is done. Ctrl-C publishes
nothing, and the comments come again the next time.

## Reply and remove

`bareblog preview` shows the short id of each published comment, the first 8 characters of its
id, with a button to copy it.

| You want to | Do this |
|---|---|
| Reply to a published comment | `bareblog comments reply <short-id> "Your reply"`, or leave the text out to write it in your editor |
| Take a comment away, with every reply under it | `bareblog comments remove <short-id>` |

Each publishes the post at once, as it was published, so an unfinished edit of it stays on your
machine. A removed comment is gone from the blog, and still in the history of git.

## Turn comments off or on, and check

| You want to | Do this |
|---|---|
| Turn comments off | `bareblog comments --off`. The form goes from every page, the script refuses comments, and the published comments stay |
| Turn them on again | `bareblog comments --on` |
| Check that the script works | `bareblog comments check` |

`--on` and `--off` publish every page. `--yes` answers yes to the question of `remove`, `--on`
and `--off`.

## When something goes wrong

| You see | It means |
|---|---|
| Comments are not connected | Run `bareblog comments connect` first |
| Nothing answers there (404), or Google refused it (403) | The address is not that of the deploy, or the deploy is not for Anyone. Deploy again as in step 3 |
| Another secret came first | The sheet was connected from another computer. Copy `~/.bareblog/comments-secret` from that computer to the same place on this one, readable by you only. Or in the sheet: Extensions, Apps Script, Project Settings, Script Properties, delete secret, and connect again |
| The script of the sheet is older than this BareBlog's | `check` wrote the newer script to a file, and says where. In the sheet: Extensions, Apps Script, paste it over the code, save, and deploy a new version under Manage deployments |
| git could not push | The comments are published on your machine. When git can push, publish again with `bareblog publish --all`. The next walk-through deletes their rows |

The rows are the comments that wait. You can read them in the sheet at any time, and delete a
row there by hand.

## What a reader can send

- A name of at most 80 characters, and a text of at most 4000, in Markdown. Links, emphasis,
  lists, quotes and code show. Pictures show as links. A link opens in a new tab.
- Google holds each comment until you approve or decline it. Turning comments on is your
  agreement to that. The page does not tell a reader. Say it on a page of your own, such as
  About, if you want readers to know.
