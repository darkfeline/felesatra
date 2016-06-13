{% extends 'base.lisp' %}

{% block base_content %}
(section
 (header
  class = "content-header"
  (h1 "{{ title }}")
  (dl
   {% if published %}
   (dt "Published")
   (dd "{{ published }}")
   {% endif %}
   {% if updated %}
   (dt "Updated")
   (dd "{{ updated }}")
   {% endif %}
   {% if category %}
   (dt "Category")
   (dd "{{ category }}")
   {% endif %})
  {% if tags %}
  (dl
   (dt "Tags")
   {% for tag in tags %}
   (dd "{{ tag }}")
   {% endfor %})
  {% endif %})

 {% block content %}
 {{ content }}
 {% endblock %})
{% endblock base_content %}
