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
    _render(args.src, args.dst)


def _render(src: str, dst: str):
    os.makedirs(os.path.dirname(dst), exist_ok=True)
    env = rendering.make_env()
    rendering.render_document(env, src, dst)


if __name__ == '__main__':
    sys.exit(main(sys.argv))
