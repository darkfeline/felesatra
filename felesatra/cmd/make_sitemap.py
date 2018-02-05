import argparse
import sys

from mir import sitemap

from felesatra import indexing


def main(argv):
    parser = argparse.ArgumentParser(prog=argv[0], description=__doc__)
    parser.add_argument('src')
    parser.add_argument('dst')
    parser.add_argument('--prefix', default='https://example.com/')
    args = parser.parse_args(argv[1:])
    with open(args.src, newline='') as f:
        entries = indexing.load(f)
        urls = [
            sitemap.URL(
                loc=args.prefix + entry.path,
                lastmod=entry.modified,
            )
            for entry in entries
        ]
    with open(args.dst, 'w') as f:
        sitemap.write_urlset(f, urls)
    return 0


if __name__ == '__main__':
    sys.exit(main(sys.argv))
