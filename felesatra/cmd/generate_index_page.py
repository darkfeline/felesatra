"""Render webpages."""

import argparse
import logging
import sys

from felesatra import indexing
from felesatra import rendering

logger = logging.getLogger(__name__)


def main(argv):
    logging.basicConfig(level='INFO')
    parser = argparse.ArgumentParser(prog=argv[0], description=__doc__)
    parser.add_argument('template')
    parser.add_argument('page_index')
    args = parser.parse_args(argv[1:])
    _generate(args.template, args.page_index)


def _generate(template: str, page_index: str):
    env = rendering.make_env()
    with open(template) as f:
        template = env.from_string(f.read())
    with open(page_index) as f:
        entries = sorted(indexing.load(f), key=lambda e: e.path)
    print(template.render({
        'pages': entries,
    }))


if __name__ == '__main__':
    sys.exit(main(sys.argv))
