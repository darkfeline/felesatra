import io

import pytest

from frelia import config


@pytest.fixture
def configfile():
    return io.StringIO('site_url: http://localhost')


def test_load(configfile):
    got = config.load(configfile)
    assert got == {
        'site_url': 'http://localhost',
        'static_dir': 'static',
        'pages_dir': 'pages',
        'template_dir': 'templates',
    }
