from frelia import config


def test_recursive_update_missing():
    base = {'cass': 'delta'}
    config._recursive_update(base, {'ion': 'earthes'})
    assert base == {
        'cass': 'delta',
        'ion': 'earthes',
    }


def test_recursive_update_nondict():
    base = {'ion': 'nay'}
    config._recursive_update(base, {'ion': 'earthes'})
    assert base == {'ion': 'earthes'}


def test_recursive_update_dict_nondict():
    base = {'ion': {'name': 'nei'}}
    config._recursive_update(base, {'ion': 'earthes'})
    assert base == {'ion': 'earthes'}


def test_recursive_update_nondict_dict():
    base = {'ion': 'earthes'}
    config._recursive_update(base, {'ion': {'name': 'nei'}})
    assert base == {'ion': {'name': 'nei'}}


def test_recursive_update_dict():
    nested = {'name': 'nei'}
    base = {'ion': nested}
    config._recursive_update(base, {'ion': {'name': 'ionasal'}})
    assert base == {'ion': {'name': 'ionasal'}}
    assert base['ion'] is nested
