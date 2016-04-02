import argparse
import os
import shutil

from felesatra import render, resources


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('src_dir')
    parser.add_argument('build_dir')
    args = parser.parse_args()

    res = resources.Resources()
    build(args.src_dir, args.dest_dir)

if __name__ == '__main__':
    main()
