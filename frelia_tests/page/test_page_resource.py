from unittest import mock
import pytest

import frelia.page


@pytest.fixture
def page():
    page_mock = mock.create_autospec(frelia.page.Page)
    page_mock.render_page.return_value = 'page content'
    return page_mock


@pytest.fixture
def page_resource(page):
    return frelia.page.PageResource('blog/page', page)


def test_build(tmpdir, page_resource):
    page_resource.build(mock.sentinel.env, str(tmpdir.join('dst')))
    indexfile = tmpdir.join('dst/blog/page/index.html')
    assert indexfile.read() == 'page content'


def test_get_dst_path(page_resource):
    got = page_resource._get_dst_path('dst')
    assert got == 'dst/blog/page/index.html'
