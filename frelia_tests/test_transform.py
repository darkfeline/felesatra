from unittest import mock

import jinja2
import pytest

from frelia import transform


class _Document:

    def __init__(self, metadata, content):
        self.metadata = metadata
        self.content = content


@pytest.fixture
def document():
    return _Document({'ion': 'nero'}, '{{yuno}}')


def test_render_jinja(document):
    assert document.metadata == {'ion': 'nero'}
    assert document.content == '{{yuno}}'

    env = jinja2.Environment()
    env.globals['yuno'] = 'miya'

    render = transform.RenderJinja(env)
    render(document)

    assert document.metadata == {'ion': 'nero'}
    assert document.content == 'miya'


def test_transformer():
    mock_func = mock.Mock()
    transformer = transform.Transformer([mock_func])

    transformer.transform(mock.sentinel.object)

    assert mock_func.mock_calls == [mock.call(mock.sentinel.object)]
