# pylint: disable=missing-docstring

import argparse

from felesatra import site


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('src_dir')
    parser.add_argument('build_dir')
    args = parser.parse_args()

    website = site.Website(args.src_dir)
    website.render(args.dest_dir)

if __name__ == '__main__':
    main()
