"""Index webpages."""

import argparse
import logging
import os
import sys

from felesatra import enja
from felesatra import indexing

logger = logging.getLogger(__name__)


def main(argv):
    logging.basicConfig(level='INFO')
    parser = argparse.ArgumentParser(prog=argv[0], description=__doc__)
    parser.add_argument('src')
    parser.add_argument('dst')
    args = parser.parse_args(argv[1:])
    entries = _index_recursively(args.src)
    with open(args.dst, 'w') as f:
        indexing.dump(entries, f)


def _index_recursively(src: str):
    entries = []
    for root, dirs, files in os.walk(src):
        for file in files:
            srcfile = os.path.join(root, file)
            logger.info('Indexing %s', srcfile)
            entries.append(_make_entry(src, srcfile))
    return entries


def _make_entry(basedir: str, srcfile: str):
    with open(srcfile) as f:
        document = enja.load(f)
    context = enja.context(document, srcfile)
    entry_path = os.path.relpath(os.path.splitext(srcfile)[0], basedir)
    return indexing.Entry(
        path=entry_path,
        title=context['title'],
        published=context['published'],
        modified=context['modified'],
    )


if __name__ == '__main__':
    sys.exit(main(sys.argv))
