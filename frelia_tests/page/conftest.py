from unittest import mock

import pytest

import frelia.page


@pytest.fixture
def doc_loader():
    class _MockDocumentLoader:
        def load(self, file):
            return mock.sentinel.document
    return _MockDocumentLoader


@pytest.fixture
def page_loader(doc_loader):
    return frelia.page.PageLoader(frelia.page.Page, doc_loader())
