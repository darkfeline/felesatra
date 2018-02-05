import os

import flask
from google.appengine.api.app_logging import AppLogsHandler

_DIR = os.path.join(os.path.abspath(os.path.dirname(__file__)), 'pages')

app = flask.Flask(__name__)
app.logger.addHandler(AppLogsHandler())


@app.route("/")
def index():
    return _load_page('index')


@app.route('/<string:path>')
def default(path):
    try:
        return _load_page(path)
    except IOError as e:
        app.logger.error('Error loading %s: %s', path, e)
        flask.abort(404)


@app.route('/<string:path>/')
def trailing_slash(path):
    return flask.redirect('/' + path)


@app.errorhandler(404)
def page_not_found(e):
    return (_load_page('404'), 404)


def _load_page(path):
    path = os.path.join(_DIR, path + '.html')
    with open(path) as f:
        return f.read()
