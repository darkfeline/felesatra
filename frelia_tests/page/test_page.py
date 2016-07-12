from unittest import mock

import jinja2
import pytest

import frelia.document
import frelia.page


def test_copy_default_context():
    Page = frelia.page.Page
    got = Page._copy_default_context()
    assert got == Page._DEFAULT_CONTEXT
    assert got is not Page._DEFAULT_CONTEXT


def test_context(page):
    assert page._context == {
        'template': 'base.html',
        'title': '',
        'cass': 'delta',
        'content': 'reincarnation',
    }


def test_template(page, environment):
    template = page._get_template(environment)
    assert template == mock.sentinel.template
    assert environment.mock_calls == [mock.call.get_template('base.html')]


def test_render_page(monkeypatch, page):
    get_template = mock.Mock(**{
        'return_value.render.return_value': mock.sentinel.rendered,
    })
    monkeypatch.setattr(frelia.page.Page, '_get_template', get_template)
    monkeypatch.setattr(frelia.page.Page, '_context', mock.sentinel.context)
    got = page.render_page(mock.sentinel.env)
    assert get_template.mock_calls == [
        mock.call(mock.sentinel.env),
        mock.call().render(mock.sentinel.context)
    ]
    assert got == mock.sentinel.rendered


@pytest.fixture
def environment():
    env = mock.create_autospec(
        jinja2.Environment,
        instance=True)
    env.get_template.return_value = mock.sentinel.template
    return env



@pytest.fixture
def document():
    return frelia.document.Document({'cass': 'delta'}, 'reincarnation')


@pytest.fixture
def page(document):
    return frelia.page.Page(document)
