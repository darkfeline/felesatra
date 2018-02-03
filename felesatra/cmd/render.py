"""Render a webpage from an Enja document."""

import argparse
import sys

import jinja2

from felesatra import enja


def main(argv):
    parser = argparse.ArgumentParser(prog=argv[0],
                                     description=__doc__)
    parser.add_argument('src')
    parser.add_argument('dst')
    args = parser.parse_args(argv[1:])
    env = _make_env()
    with open(args.src) as f:
        document = enja.load(f)
    template = _document_template(env, document)
    context = _document_context(document)
    html = template.render(context)
    with open(args.dst, 'w') as f:
        f.write(html)
    return 0


def _make_env():
    env = jinja2.Environment(
        loader=jinja2.PackageLoader('felesatra', 'templates'),
        trim_blocks=True,
        lstrip_blocks=True,
        auto_reload=False)
    return env


def _document_template(env, document):
    return env.get_template(document.header.get('template', 'content_page.html'))


def _document_context(document):
    context = document.header.copy()
    context['content'] = document.body
    return context


if __name__ == '__main__':
    sys.exit(main(sys.argv))
