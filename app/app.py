import flask

import os

app = flask.Flask(__name__)


@app.route("/")
def index():
    return _load_page('index')


@app.route('/<string:path>')
def default(path):
    try:
        return _load_page(path)
    except OSError:
        flask.abort(404)


@app.route('/<string:path>/')
def trailing_slash(path):
    return flask.redirect('/' + path)


@app.errorhandler(404)
def page_not_found(e):
    return (_load_page('404'), 404)


def _load_page(path):
    path = os.path.join(__file__, '%s.html' % path)
    with open(path) as f:
        return f.read()
