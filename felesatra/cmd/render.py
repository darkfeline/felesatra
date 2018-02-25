"""Render webpages."""

import argparse
import logging
import os
import sys

from felesatra import rendering

logger = logging.getLogger(__name__)


def main(argv):
    logging.basicConfig(level='INFO')
    parser = argparse.ArgumentParser(prog=argv[0], description=__doc__)
    parser.add_argument('src')
    parser.add_argument('dst')
    args = parser.parse_args(argv[1:])
    _render_recursively(args.src, args.dst)


def _render_recursively(src: str, dst: str):
    env = rendering.make_env()
    for root, dirs, files in os.walk(src):
        for file in files:
            srcfile = os.path.join(root, file)
            relpath = os.path.relpath(srcfile, src)
            dstfile = os.path.join(dst, relpath)
            logger.info('Rendering %s to %s', srcfile, dstfile)
            os.makedirs(os.path.dirname(dstfile), exist_ok=True)
            rendering.render_document(env, srcfile, dstfile)


if __name__ == '__main__':
    sys.exit(main(sys.argv))
