# pylint: disable=missing-docstring

import argparse
import logging

from felesatra.resources.site import Website


def main():
    logging.basicConfig(level='DEBUG')

    parser = argparse.ArgumentParser()
    parser.add_argument('src_dir')
    parser.add_argument('build_dir')
    parser.add_argument('--site-url', default='https://www.felesatra.moe')
    args = parser.parse_args()

    website = Website(args.src_dir, args.site_url)
    website.render(website.env, args.build_dir)

if __name__ == '__main__':
    main()
