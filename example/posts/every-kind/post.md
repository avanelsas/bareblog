# Every kind of block

This post holds one of each kind of block. A paragraph can have **strong** text, *emphasis*,
`inline code`, ~~text that is struck~~, a [link](https://github.com/avanelsas/baredom), and a
footnote[^first]. A line can also break  
where the author asks for it. A small image ![a boat](boat.svg) can stand in a line.

## A heading of the second level

### And one of the third

> A quote can hold more than one paragraph.
>
> It can also hold **what a paragraph holds**, and a second footnote[^second]. A note can be
> used more than once[^first].

A list:

- A first item.
- A second item, with a list in it:
  - below the second item,
  - and once more.
- A third item.

A numbered list that starts at three:

3. Read the Markdown.
4. Turn it into blocks.
5. Show the blocks.

A photo:

![The harbour at dawn, with a boat on the water](harbour.svg)

Code in a language that `x-code` colours:

```js
const greeting = name => `Hello, ${name}.`;
console.log(greeting("reader"));
```

Code in Clojure:

```clojure
(defn greeting [name]
  (str "Hello, " name "."))
```

---

A table:

| Kind      | Shown as        | Count |
|:----------|:---------------:|------:|
| text      | a paragraph     |    12 |
| code      | `x-code`        |     3 |
| *table*   | `x-table`       |     1 |

HTML that is typed into a post is shown, not run. In a sentence it looks like <kbd>this</kbd>,
and on lines of its own like this:

<details>
  <summary>More</summary>
  Hidden text.
</details>

HTML in a fence marked `live` runs too, in a frame of its own, above its code:

```html live
<x-badge text="New" variant="success"></x-badge>
<x-badge text="Draft" variant="warning"></x-badge>
```

A post names its tags on its last line, as this one does. An underscore is a space.

#bareblog #reference #every_kind

[^first]: The first note. It has a [link](https://github.com/avanelsas/bareblog) of its own.
[^second]: The second note.
