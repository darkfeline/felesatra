import pytest

import frelia.page


class _MockPage:

    @property
    def rendered_page(self):
        return 'page content'


@pytest.fixture
def page():
    return _MockPage()


@pytest.fixture
def page_resource(page):
    return frelia.page.PageResource('blog/page', page)


def test_build(tmpdir, page_resource):
    page_resource.build(str(tmpdir.join('dst')))
    indexfile = tmpdir.join('dst/blog/page/index.html')
    assert indexfile.read() == 'page content'


def test_get_dst_path(page_resource):
    got = page_resource._get_dst_path('dst')
    assert got == 'dst/blog/page/index.html'
