{% extends 'html_base.lisp' %}

{% from 'macros.lisp' import abs_url, pgpkey %}

{% block head %}
(meta name = "viewport"
      content = "width = device-width, initial-scale = 1")
(link rel = "stylesheet"
      type = "text/css"
      href = "{{ abs_url('css/base.css') }}")
(link rel = "stylesheet"
      type = "text/css"
      href = "{{ abs_url('css/site.css') }}")
(link rel = "icon"
      type = "image/png"
      href = "{{ abs_url('img/site/favicon.png') }}")

{% if js_tablesorter %}
(script src = "https://code.jquery.com/jquery-1.11.3.min.js")
(script src = "{{ abs_url('js/tablesorter.js') }}")
(script "$(document).ready(function() { $('table.tablesorter').tablesorter(); });")
{% endif %}
{% endblock head %}

{% block title %}"{{ title }}"{% endblock %}

{% block body %}
(header
 id = "site-header"
 (h1 id = "site-title"
     (a href = "{{ abs_url('/') }}"
        "Feles Atra"))
 (ul id = "site-nav"
     {% for name, ref in site.nav.items() %}
     (li (a href = "{{ abs_url(ref) }}" "{{ name }}"))
     {% endfor %}
     (li (a href = "{{ abs_url('atom.xml') }}"
            (img src = "{{ abs_url('img/site/feed-icon-14x14.png') }}"
                 alt = "RSS feed")))))

{% block base_content %}
{{ content }}
{% endblock %}

(footer
 id = "site-footer"
 (address "The webmaster for this site is "
          {% call pgpkey() %}"darkfeline"{% endcall %}
          ".")
 (div
  id = "site-copyright"
  (p "Feles Atra by Allen Li is licensed under a "
     (a rel = "license"
        href = "https://creativecommons.org/licenses/by-sa/4.0/"
        "Creative Commons Attribution-ShareAlike 4.0 International License")
     ".")
  (a rel = "license"
     href = "https://creativecommons.org/licenses/by-sa/4.0/"
     (img alt = "Creative Commons License"
          class = "centered"
          src = "https://i.creativecommons.org/l/by-sa/4.0/88x31.png"))))
{% endblock body %}
