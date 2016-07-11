"""Simple static web server for local development."""

import argparse
import logging
import os

from flask import Flask

logger = logging.getLogger(__name__)


def make_app(serv_dir):
    app = Flask(__name__, static_folder=serv_dir)

    def getpath(path, *paths):
        return os.path.join(serv_dir, path, *paths)

    @app.route('/')
    def root():
        return app.send_static_file('index.html')

    @app.route('/<path:path>')
    def static_proxy(path):
        if os.path.isdir(getpath(path)):
            path = os.path.join(path, 'index.html')
        logger.debug('path %s', path)
        return app.send_static_file(path)

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
