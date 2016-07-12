from unittest import mock

import pytest

import frelia.page


def test_load_pages(tmpdir, page_resource_loader):
    pagedir = tmpdir.mkdir('pages')
    pagedir.mkdir('blog').join('page.html').write('')

    got = list(page_resource_loader.load_pages(str(pagedir)))

    assert len(got) == 1
    resource = got[0]
    assert resource.path == 'blog/page'
    assert resource.page.document == mock.sentinel.document


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
