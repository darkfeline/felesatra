from unittest import mock

import jinja2
import pytest

import frelia.page
import frelia.transform


def test_render_jinja(document):
    assert document.metadata == {'ion': 'nero'}
    assert document.content == '{{yuno}}'

    env = jinja2.Environment()
    env.globals['yuno'] = 'miya'

    render = frelia.transform.RenderJinja(env)
    render(document)

    assert document.metadata == {'ion': 'nero'}
    assert document.content == 'miya'


def test_document_page_transform(document):
    page = frelia.page.Page('foo', document)
    document_func = mock.Mock()
    page_func = frelia.transform.DocumentPageTransform(document_func)
    page_func(page)
    assert document_func.mock_calls == [mock.call(document)]


def test_transform_group():
    mock_func = mock.Mock()
    transform = frelia.transform.TransformGroup([mock_func])

    transform(mock.sentinel.object)

    assert mock_func.mock_calls == [mock.call(mock.sentinel.object)]


class _Document:

    def __init__(self, metadata, content):
        self.metadata = metadata
        self.content = content


@pytest.fixture
def document():
    return _Document({'ion': 'nero'}, '{{yuno}}')
