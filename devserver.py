"""Simple static web server for local development."""

import argparse
import logging
import os

from flask import Flask

# pylint: disable=missing-docstring

logger = logging.getLogger(__name__)


def make_app(serv_dir):
    # pylint: disable=unused-variable
    app = Flask(__name__)

    def getpath(path, *paths):
        return os.path.join(serv_dir, path, *paths)

    @app.route('/')
    def root():
        realpath = getpath('index.html')
        logger.debug('Path %s', realpath)
        return app.send_static_file(realpath)

    @app.route('/<path:path>')
    def static_proxy(path):
        realpath = getpath(path)
        if os.path.isdir(realpath):
            realpath = getpath(path, 'index.html')
        logger.debug('Path %s', realpath)
        return app.send_static_file(realpath)

    return app


def main():
    logging.basicConfig(level='DEBUG')
    parser = argparse.ArgumentParser()
    parser.add_argument('serv_dir')
    args = parser.parse_args()
    app = make_app(args.serv_dir)
    app.run()

if __name__ == '__main__':
    main()
