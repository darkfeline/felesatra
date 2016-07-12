from unittest import mock

import jinja2
import pytest

import frelia.document


def test_raw_document_loader(enja_document_class):
    loader = frelia.document.RawDocumentLoader(enja_document_class)
    got = loader.load(mock.sentinel.file)
    assert got.metadata == {'ion': 'nero'}
    assert got.content == 'content'
    assert got.loader is loader


@pytest.fixture
def enja_document_class():
    MockEnjaDocument = mock.create_autospec(frelia.enja.EnjaDocument)
    doc = frelia.enja.EnjaDocument({'ion': 'nero'}, 'content')
    MockEnjaDocument.load.return_value = doc
    return MockEnjaDocument


class _Loader(frelia.document.JinjaDocumentLoader):

    def _load(self, file):
        return frelia.document.Document(
            {'ion': 'nero'},
            '{{yuno}}',
            loader=self)


def test_jinja_document_loader():
    env = jinja2.Environment()
    env.globals['yuno'] = 'miya'

    loader = _Loader(env)
    got = loader.load(mock.sentinel.file)

    assert got.metadata == {'ion': 'nero'}
    assert got.content == 'miya'
    assert got.loader is loader
