// Code generated by "binpack -name baseTemplate base.html"; DO NOT EDIT.

package templates

const baseTemplate = "<!DOCTYPE HTML>\n<html lang=\"en\">\n  <head>\n    <meta charset=\"UTF-8\">\n    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n    <link rel=\"stylesheet\" type=\"text/css\" href=\"/css/base.css\">\n    <link rel=\"stylesheet\" type=\"text/css\" href=\"/css/site.css\">\n    <link rel=\"icon\" type=\"image/png\" href=\"/img/site/favicon.png\">\n    <title>{{.title}}</title>\n  </head>\n  <body>\n    <header id=\"site-header\">\n      <h1 id=\"site-title\"><a href=\"/\">Feles Atra</a></h1>\n    </header>\n    <section>\n      <header class=\"content-header\">\n        <h1>{{.title}}</h1>\n        <dl>\n          {{- if .published}}\n          <dt>Published</dt>\n          <dd>{{.published}}</dd>\n          {{- end}}\n          {{- if .modified}}\n          <dt>Last modified</dt>\n          <dd>{{.modified}}</dd>\n          {{- end}}\n        </dl>\n      </header>\n      {{.body}}\n    </section>\n\n    <footer id=\"site-footer\">\n      <address>\n        <a href=\"https://keys.openpgp.org/search?q=E9397FA17F2501B5EF7344B9871AC6C82D45F74D\">Webmaster</a>\n      </address>\n      <a href=\"/atom.xml\">Atom feed</a>\n      <div id=\"site-copyright\">\n        <p>\n          <span xmlns:dct=\"https://purl.org/dc/terms/\" property=\"dct:title\">Feles Atra</span> by\n          <a xmlns:cc=\"https://creativecommons.org/ns#\"\n             href=\"https://www.felesatra.moe\" property=\"cc:attributionName\"\n             rel=\"cc:attributionURL\">Allen Li</a> is licensed under a\n          <a rel=\"license\"\n             href=\"https://creativecommons.org/licenses/by-sa/4.0/\">Creative Commons\n            Attribution-ShareAlike 4.0 International License</a>.\n        </p>\n        <a rel=\"license\" href=\"https://creativecommons.org/licenses/by-sa/4.0/\">\n          <img alt=\"Creative Commons License\" class=\"centered\"\n               src=\"https://i.creativecommons.org/l/by-sa/4.0/88x31.png\">\n        </a>\n      </div>\n    </footer>\n  </body>\n</html>\n"