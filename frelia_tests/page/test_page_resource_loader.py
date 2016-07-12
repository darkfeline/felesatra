from unittest import mock

import pytest

import frelia.page


def test_load_pages(tmpdir, page_loader):
    tmpdir.mkdir('blog').join('page.html')

    # pages = list(frelia.page.load_pages(page_loader))


def test_get_page_resource_path(page_resource_loader):
    got = page_resource_loader._get_page_resource_path('pages', 'pages/blog/post.html')
    assert got == 'blog/post'


@pytest.fixture
def page_resource_loader(page_loader):
    return frelia.page.PageResourceLoader(
        page_loader,
        frelia.page.PageResource)


def test_load_page(page_loader):
    page = page_loader.load(mock.sentinel.file)
    assert isinstance(page, frelia.page.Page)
    assert page.document is mock.sentinel.document
