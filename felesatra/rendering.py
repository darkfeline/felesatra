import jinja2

from felesatra import enja


def make_env():
    env = jinja2.Environment(
        loader=jinja2.PackageLoader('felesatra', 'templates'),
        trim_blocks=True,
        lstrip_blocks=True,
        auto_reload=False)
    return env


def render_document(env, src: str, dst: str):
    with open(src) as f:
        document = enja.load(f)
    template = _document_template(env, document)
    context = enja.context(document, src)
    html = template.render(context)
    with open(dst, 'w') as f:
        f.write(html)


def _document_template(env, document):
    return env.get_template(document.header.get('template', 'site_content.html'))
