import os
import logging

import flask
from google.appengine.api.app_logging import AppLogsHandler

_DIR = os.path.join(os.path.abspath(os.path.dirname(__file__)), 'pages')

logging.getLogger().addHandler(AppLogsHandler())

app = flask.Flask(__name__)


@app.route("/")
def index():
    return _load_page('index')


@app.route('/<path:path>')
def default(path):
    return _load_page(path)


@app.route('/<path:path>/')
def trailing_slash(path):
    return flask.redirect('/' + path)


@app.errorhandler(404)
def page_not_found(e):
    return _load_page_unsafe('404'), 404


def _load_page(path):
    try:
        return _load_page_unsafe(path)
    except IOError as e:
        logging.error('Error loading %s: %s', path, e)
        flask.abort(404)


def _load_page_unsafe(path):
    path = os.path.join(_DIR, path + '.html')
    with open(path) as f:
        return f.read()
