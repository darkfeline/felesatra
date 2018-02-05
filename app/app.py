import os

import flask
from google.appengine.api.app_logging import AppLogsHandler

_WWWDIR = os.path.join(os.path.abspath(os.path.dirname(__file__)), 'www')

app = flask.Flask(__name__)
app.logger.addHandler(AppLogsHandler())


@app.route("/")
def index():
    return _load_page('index.html')


@app.route('/<string:path>')
def default(path):
    if not os.path.splitext(path)[1]:
        path = path + '.html'
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
    return (_load_page('404.html'), 404)


def _load_page(path):
    path = os.path.join(_WWWDIR, path)
    with open(path) as f:
        return f.read()
