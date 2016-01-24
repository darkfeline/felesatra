---
title: "About"
date: "2015-08-27"
slug: "about"
publishdate: "2015-08-27"
modified: "2016-01-23"
subjects:
  - "meta"
toc: true
---

<aside>
  {{< toc >}}
</aside>

## About me

I like it when things are done the right way.  I do not like it when things are
done the wrong way because that is the leading cause of every single thing gone
wrong in society, both now and forever.

My interests include:

- single player video games
- anime
- music
- scripting/programming
- system administration/management
- hacking ("MacGyvering")

Some personal links:

- [my projects]({{< ref "projects.md" >}})
- [my Github page][Github]

[Github]: https://github.com/darkfeline

## About this site

This is my personal website, and thus it hosts content that I feel like hosting.
For the most part, this is limited to my own blog posts and assorted pages that
I have written and now maintain, but if you have any content that you would like
me to host, feel free to contact me; I will host it if I like it.

### Site structure

Every page on this site belongs in one and only one section.  Each section can
be thought of as a mini-site or mini-blog, containing related content.

<dl>
  <dt><a href="/document">document</a></dt>
  <dd>Formal prose, technical documents, and the like.  Pages that will be
    repeatedly updated in place.</dd>
  <dt><a href="/meta">meta</a></dt>
  <dd>News about the site itself.</dd>
  <dt><a href="/sankaku">sankaku</a></dt>
  <dd>Posts about things related to anime, manga, visual novels, and RPGs.  More
    information about this concept can be found on <a
    href="https://www.sankakucomplex.com/about/">this (unfortunately NSFW)
    site</a>.</dd>
  <dt><a href="/tech">tech</a></dt>
  <dd>Anything related to technology or news in general, in the vein of Slashdot
    or <a href="https://soylentnews.org/">SoylentNews</a>.</dd>
  <dt><a href="/transcription">transcription</a></dt>
  <dd>Music transcriptions</dd>
</dl>

In addition, pages are organized with tagging metadata.  Whereas other sites
might use categories and tags, this site uses subjects and contents.
<dfn>Subjects</dfn> describe the subject matters to which the page is relevant,
and <dfn>contents</dfn> describe the content formats contained in the page.

For example, an anime review post is about the subject matter [anime]({{< absURL
"subjects/anime" >}}) and contains content of the [review]({{< absURL
"contents/review" >}}) type.  A news post about this site has [meta]({{< absURL
"subjects/meta" >}}) subject matter (since it is talking about itself), and the
content it contains is of the type [news]({{< absURL "contents/news" >}}).

Subjects describe *what* the page is about, and contents describe *how* the page
talks about its subjects.

### RSS

A general RSS feed can be found on the homepage.  Feeds restricted to a given
section, subject, or content can be found through the
[site index]({{< ref "site-index.md" >}}).

### Design principles

This site probably doesn't look like what you're used to.  That's because the
sites you are familiar with follow questionable design principles.  Here are the
design principles for this site:

- Simple is best.
- Content is king.
- Pages should be HTML documents that are viewable without CSS or JavaScript.

To make a point, compare
[every fucking website](http://everyfuckingwebsite.com/) with
[a motherfucking website](http://motherfuckingwebsite.com/).

### SSL/TLS

This site uses a self-signed certificate.  The certificate can be found
[here]({{< absURL "files/cert/www.felesatra.moe.crt.pem" >}}), and the signature
for the certificate, made with [my public key][key] is [here]({{< absURL
"files/cert/www.felesatra.moe.crt.pem.sig" >}}).  After verifying that the
certificate is signed by me, you can add it to your browser as trusted.

### Page dates

Each page has up to three dates associated with it: sort date, publish date, and
last modified date.

The publish date, if present, indicates when the page was published.  The last
modified date, if present, indicates when the page was last modified.

The sort date is used for ordering pages on various lists.  Generally speaking,
the sort date serves either as the publish date or last modified date.  If a
modification requires that the page be bumped up the page ordering, the sort
date will be updated accordingly.

### Licensing

The content on this site is licensed under a <a rel="license"
  href="http://creativecommons.org/licenses/by-sa/4.0/">Creative Commons
  Attribution-ShareAlike 4.0 International License</a>.

## Contact

You can contact me via email at [darkfeline@felesatra.moe][email].  I would
prefer you use [my public key][key] when doing so.

[email]: mailto:darkfeline@felesatra.moe
[key]: https://sks-keyservers.net/pks/lookup?op=get&search=0x871AC6C82D45F74D
