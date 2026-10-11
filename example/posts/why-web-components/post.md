---
date: 2026-10-02
tags: web
---

# Why web components

A web component is an element the browser already knows how to run. It needs no framework
to exist, and it keeps working when the framework around it changes.

## What changed my mind

For years I treated them as a *curiosity*. Two things changed that:

The first is that every browser now runs them the same way. The second is that a component
can be **stateless**: what it shows is a function of its attributes, and nothing else.

## What they are not

They are not an application framework. Something still has to decide what the page shows.
That part is small when the components do the drawing, and it is what
[BareMirror](https://github.com/avanelsas/baredom/blob/main/docs/baremirror.md) is for.
