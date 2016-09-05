from unittest import mock

import pytest

import frelia.jinja


@pytest.fixture
def filter_module():
    module = type('_FilterModule', (), {})()
    module.__all__ = ['shurelia', 'frelia', 'tyria']
    sentinel = mock.sentinel
    module.shurelia = sentinel.shurelia
    module.frelia = sentinel.frelia
    module.tyria = sentinel.tyria
    return module


def test_load_filters_from_module(filter_module):
    """Test _load_filters_from_module()."""
    got = frelia.jinja._load_filters_from_module(filter_module)
    sentinel = mock.sentinel
    assert got == {
        'shurelia': sentinel.shurelia,
        'frelia': sentinel.frelia,
        'tyria': sentinel.tyria,
    }
