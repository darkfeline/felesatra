from unittest import mock

import jinja2
import pytest

import frelia.document


def test_raw_document_loader(enja_document_class):
    loader = frelia.document.RawDocumentLoader(enja_document_class)
    got = loader.load(mock.sentinel.file)
    assert got == frelia.document.Document({'ion': 'nero'}, 'content')
    assert got.loader is loader


@pytest.fixture
def enja_document():
    return frelia.enja.EnjaDocument({'ion': 'nero'}, 'content')


@pytest.fixture
def enja_document_class(enja_document):
    MockEnjaDocument = mock.create_autospec(frelia.enja.EnjaDocument)
    MockEnjaDocument.load.return_value = enja_document
    return MockEnjaDocument


@pytest.fixture
def jinja_loader_class():
    class Loader(frelia.document.JinjaDocumentLoader):
        def _load(self, file):
            return frelia.document.Document(
                {'ion': 'nero'},
                '{{yuno}}',
                loader=self)
    return Loader


def test_jinja_document_loader(jinja_loader_class):
    env = jinja2.Environment()
    env.globals['yuno'] = 'miya'

    loader = jinja_loader_class(env)
    got = loader.load(mock.sentinel.file)

    assert got.metadata == {'ion': 'nero'}
    assert got.content == 'miya'
    assert got.loader is loader
