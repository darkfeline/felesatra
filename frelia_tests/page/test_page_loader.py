from unittest import mock

import pytest

import frelia.page


def test_load_pages(tmpdir, page_loader):
    tmpdir.mkdir('blog').join('page.html')

    # pages = list(frelia.page.load_pages(page_loader))


def test_load_page(page_loader):
    page = page_loader.load(mock.sentinel.file)
    assert isinstance(page, frelia.page.Page)
    assert page.document is mock.sentinel.document


@pytest.fixture
def doc_loader():
    class _MockDocumentLoader:
        def load(self, file):
            return mock.sentinel.document
    return _MockDocumentLoader


@pytest.fixture
def page_loader(doc_loader):
    return frelia.page.PageLoader(frelia.page.Page, doc_loader())
