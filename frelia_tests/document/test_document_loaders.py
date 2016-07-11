from unittest import mock

import jinja2

import frelia.enja
import frelia.document


def test_enja_document_loader(monkeypatch):
    doc = frelia.enja.EnjaDocument({'ion': 'nero'}, 'content')
    load_mock = mock.Mock(return_value=doc)
    monkeypatch.setattr(
        frelia.enja.EnjaDocument,
        'load',
        load_mock)

    loader = frelia.document.EnjaDocumentLoader()
    got = loader.load(mock.sentinel.file)

    assert got == frelia.document.Document(
        mock.sentinel.loader,
        {'ion': 'nero'},
        'content')
    load_mock.assert_called_once_with(mock.sentinel.file)


def test_jinja_document_loader():
    env = jinja2.Environment()
    env.globals['yuno'] = 'miya'

    class Loader(frelia.document.JinjaDocumentLoader):

        def _load(self, file):
            return frelia.document.Document(self, {'ion': 'nero'}, '{{yuno}}')

    loader = Loader(env)
    got = loader.load(mock.sentinel.file)

    assert got.metadata == {'ion': 'nero'}
    assert got.content == 'miya'
