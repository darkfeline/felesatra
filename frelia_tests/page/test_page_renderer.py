from unittest import mock

import pytest

import frelia.page


def test_render(tmpdir, document_renderer, page):
    target_dir = str(tmpdir.join('dst'))
    renderer = frelia.page.PageRenderer(document_renderer, target_dir)

    renderer.render(page)
    rendered_file = tmpdir.join('dst/blog/page')
    assert rendered_file.read() == 'rendered content'


@pytest.fixture
def page(document):
    return frelia.page.Page('blog/page', document)


@pytest.fixture
def document_renderer():
    renderer = mock.NonCallableMock(['render'])
    renderer.render.return_value = 'rendered content'
    return renderer
