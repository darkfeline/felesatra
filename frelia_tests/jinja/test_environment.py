import frelia.jinja


def test_default_option_values():
    """Test default option values."""
    env = frelia.jinja.Environment()
    assert env.trim_blocks is True
    assert env.lstrip_blocks is True
    assert env.auto_reload is False


def test_set_custom_option_true():
    env = frelia.jinja.Environment(trim_blocks=True)
    assert env.trim_blocks is True


def test_set_custom_option_false():
    env = frelia.jinja.Environment(trim_blocks=False)
    assert env.trim_blocks is False


def test_copy_default_options():
    """Test _copy_default_options() method."""
    default_options = frelia.jinja.Environment._Environment__DEFAULT_OPTIONS
    got = frelia.jinja.Environment._copy_default_options()
    assert got == default_options
    assert got is not default_options


def test_replace_option(monkeypatch):
    monkeypatch.setattr(
        frelia.jinja.Environment,
        '_Environment__DEFAULT_OPTIONS',
        {'shurelia': 'bunnies'})
    got = frelia.jinja.Environment._customize_options({'shurelia': 'mir'})
    assert got == {'shurelia': 'mir'}


def test_add_option(monkeypatch):
    monkeypatch.setattr(
        frelia.jinja.Environment,
        '_Environment__DEFAULT_OPTIONS',
        {'shurelia': 'bunnies'})
    got = frelia.jinja.Environment._customize_options({'mir': 'bully'})
    assert got == {'shurelia': 'bunnies', 'mir': 'bully'}
