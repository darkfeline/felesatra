# pylint: disable=missing-docstring

import argparse
import logging
import os

from felesatra.env import make_env
from felesatra.resources import Website


def sitepath():
    return os.path.join(
        os.path.dirname(__file__),
        'site',
    )


def main():
    logging.basicConfig(level='DEBUG')

    parser = argparse.ArgumentParser()
    parser.add_argument('build_dir')
    parser.add_argument('--site-url', default='https://www.felesatra.moe')
    args = parser.parse_args()

    src_dir = sitepath()
    env = make_env(args.site_url, src_dir)
    website = Website(src_dir)
    website.walk(env)
    website.render(env, args.build_dir)

if __name__ == '__main__':
    main()
