template: content_page.lisp
title: Projects
published: 2015-08-28
modified: 2016-02-18
---

{% from "macros.lisp" import abs_url %}

(p "A somewhat comprehensive list of my publicly maintained projects for
personal reference.  Most of these are presentable (stable, functioning, usable,
etc.)  so if you find them useful, feel free to use them.  Contact me if you
need a free software license and one isn't present.")

{% macro project(name, link) %}
(dt (a href = "{{ link }}" "{{ name }}"))
(dd "{{ caller() }}")
{% endmacro %}

(dl
 {% call project('linca', 'https://github.com/darkfeline/linca') -%}
 A simple program for watching directories and linking new files.
 {%- endcall %}

 {% call project('winenv', 'https://github.com/darkfeline/winenv') -%}
 An environment manager for Wine.
 {%- endcall %}

 {% call project('Teiwaz Project', 'https://teiwaz-project.github.io/') -%}
 An ambitious endeavor to seriously pursue the development of strong AI and related topics.
 {%- endcall %}

 {% call project('chronoplot', 'https://darkfeline.github.io/chronoplot/') -%}
 Timeline maker written in Python.
 {%- endcall %}

 {% call project('animanager', abs_url('animanager/')) -%}
 Command line anime tracker/manager.
 {%- endcall %}

 {% call project('euterpe', 'https://github.com/darkfeline/euterpe') -%}
 A simple Python 3 program for syncing a music library to a music player.
 {%- endcall %}

 {% call project('binasphere', 'https://github.com/darkfeline/binasphere') -%}
 A Python script for converting stanzas to and from Hymmnos Binasphere format and
 for general interweaving of text.
 {%- endcall %}

 {% call project('pymsmtpq', 'https://github.com/darkfeline/pymsmtpq') -%}
 Asynchronous email queuing for msmtp.
 {%- endcall %}

 {% call project('dantalian', 'https://darkfeline.github.io/dantalian/') -%}
 Tools for tagging files with hard links.
 {%- endcall %}

 {% call project('torrent-preview', 'https://github.com/darkfeline/torrent-preview') -%}
 Simple torrent file previewing script.
 {%- endcall %}

 {% call project('sadpanda_downloader', 'https://github.com/darkfeline/sadpanda_downloader') -%}
 Doujinshi scraper/downloader for sadpanda.
 {%- endcall %}

 {% call project('fakku_downloader', 'https://github.com/darkfeline/fakku_downloader') -%}
 Doujinshi scraper/downloader for Fakku.
 {%- endcall %}
 )