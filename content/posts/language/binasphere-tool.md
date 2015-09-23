---
title: Binasphere tool
date: 2015-01-12
publishdate: 2015-01-12
subjects:
  - "language"
  - "programming"
contents:
  - "news"
---

I wrote a quick Python script to help construct all of the examples in
[my Quatrasphere post][quatrasphere].  After cleaning it up a bit, I posted it
to [Github] in case anyone would ever need to play with some Hymmnos.  The rest
of the text processing in my post was done with Vim macros, which I won't
bother posting since the macros were done rather haphazardly.

[quatrasphere]: {{< ref "hymmnos-quatrasphere.md" >}}
[Github]: https://github.com/darkfeline/binasphere

Note that the tool itself is a little overkill, as clever use of UNIX standard
utilities can do the same thing (for example, [join]), but digging through
manpages is no fun.

[join]: http://en.wikipedia.org/wiki/Join_(Unix)
