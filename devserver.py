"""Simple static web server for local development."""

# XXX cherrypy

import argparse
import logging
import os

import flask

logger = logging.getLogger(__name__)


def make_app(serv_dir):
    app = flask.Flask(__name__)

    def getpath(path, *paths):
        return os.path.join(serv_dir, path, *paths)

    @app.route('/')
    def root():
        return flask.send_from_directory(serv_dir, 'index.html')

    def no_extension(path):
        return not os.path.splitext(path)[1]

    @app.route('/<path:path>')
    def static_proxy(path):
        if os.path.isdir(getpath(path)):
            path = os.path.join(path, 'index.html')
        logger.debug('path %s', path)
        if no_extension(path):
            return flask.send_from_directory(serv_dir, path, mimetype='text/html')
        else:
            return flask.send_from_directory(serv_dir, path)

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
