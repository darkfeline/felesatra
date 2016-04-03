# pylint: disable=missing-docstring

import argparse
import logging
import os

from felesatra.resources.site import Website
from felesatra.env import make_env


def getsitepath():
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

    env = make_env(args.site_url)
    website = Website(getsitepath())
    website.render(env, args.build_dir)

if __name__ == '__main__':
    main()
