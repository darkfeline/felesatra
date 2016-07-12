from unittest import mock

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


def test_template(page):
    page.env.get_template.return_value = mock.sentinel.template
    template = page._template
    assert template == mock.sentinel.template
    assert page.env.mock_calls == [mock.call.get_template('base.html')]


def test_rendered_page(monkeypatch, page):
    template = mock.Mock()
    context = mock.Mock()
    monkeypatch.setattr(frelia.page.Page, '_template', template)
    monkeypatch.setattr(frelia.page.Page, '_context', context)
    got = page.rendered_page
    assert template.mock_calls == [mock.call.render(context)]
    assert context.mock_calls == []


@pytest.fixture
def document():
    return frelia.document.Document({'cass': 'delta'}, 'reincarnation')


@pytest.fixture
def mockenv():
    return mock.Mock()


@pytest.fixture
def page(mockenv, document):
    return frelia.page.Page(mockenv, document)
