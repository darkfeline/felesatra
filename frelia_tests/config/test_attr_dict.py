import pytest

from frelia import config


@pytest.fixture
def attrdict():
    return config._AttrDict({
        'ion': {'name': 'nei'},
        'cass': 'casty',
    })


def test_get_nondict(attrdict):
    got = attrdict.cass
    assert got == 'casty'


def test_get_dict(attrdict):
    got = attrdict.ion
    assert got == {'name': 'nei'}
    assert isinstance(got, config._AttrDict)
