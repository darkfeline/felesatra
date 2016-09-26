"""Entry point for felesatra."""

import argparse
import cProfile
import logging

import coloredlogs

import felesatra.build as build
import felesatra.config as config

logger = logging.getLogger(__name__)


def main():
    """Entry point.

    Calling this starts the program.
    """
    init_logging()
    args = parse_args()
    conf = config.load_config(args.config)
    if args.profile:
        cProfile.run('build.build(conf)', sort='cumtime')
    else:
        build.build(conf)


def init_logging():
    """Initialize logging."""
    coloredlogs.install(
        level='INFO',
        fmt='%(asctime)s %(name)s %(levelname)s %(message)s')


def parse_args():
    """Parse arguments."""
    parser = argparse.ArgumentParser()
    parser.add_argument('build_dir')
    parser.add_argument('--config', default='config.yaml')
    parser.add_argument('--profile', action='store_true')
    return parser.parse_args()
