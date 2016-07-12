import pytest


@pytest.fixture
def document():
    return _Document({'sophie': 'prachta'}, 'girl meets girl')


class _Document:

    def __init__(self, metadata, content):
        self.metadata = metadata
        self.content = content
