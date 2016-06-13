{% macro abs_url(ref) -%}
{{ ref|urljoin(site.url) }}
{%- endmacro %}

{% macro imgfig(src, alt) %}
(figure
 (img src = "{{ abs_url(src) }}"
      alt = "{{ alt }}"))
{% endmacro %}

{% macro pgpkey() %}
(a href = "https://sks-keyservers.net/pks/lookup?op=get&search=0x871AC6C82D45F74D"
   {{ caller() }})
{% endmacro %}

{% macro page_li(page) %}
(li (span class = "aside"
          "{{ page.updated.date() }}")
    " "
    (a href = "{{ page.href }}"
       {{ page.title }}))
{% endmacro %}

{% macro codeblock() %}
(pre (code "{{ caller()|e }}"))
{% endmacro %}

{% macro code(text) %}
(code "{{ text|e }}")
{% endmacro %}
