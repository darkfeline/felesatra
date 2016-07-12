from unittest import mock

import pytest

import frelia.page


def test_load_page(pageloader, mockenv, mockdoc):
    page = pageloader.load(mock.sentinel.file)
    assert page.env is mockenv
    assert page.document is mockdoc


@pytest.fixture
def mockdoc():
    return mock.sentinel.document


@pytest.fixture
def docloader(mockdoc):
    class _MockDocumentLoader:
        def load(self, file):
            return mockdoc
    return _MockDocumentLoader


@pytest.fixture
def mockenv():
    return mock.sentinel.env


@pytest.fixture
def pageloader(mockenv, docloader):
    return frelia.page.PageLoader(mockenv, docloader())
