from unittest import mock

import frelia.page


def test_load_page(page_loader):
    page = page_loader.load(mock.sentinel.file)
    assert isinstance(page, frelia.page.Page)
    assert page.document is mock.sentinel.document
