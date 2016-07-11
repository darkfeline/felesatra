# pylint: disable=protected-access

import unittest
from unittest import mock

import frelia.jinja


class LoadFiltersTestCase(unittest.TestCase):

    def test_load_filters_from_module(self):
        """Test _load_filters_from_module()."""
        module = mock.NonCallableMock()
        module.__all__ = ['shurelia', 'frelia', 'tyria']
        module.shurelia = mock.sentinel.shurelia
        module.frelia = mock.sentinel.frelia
        module.tyria = mock.sentinel.tyria

        got = frelia.jinja._load_filters_from_module(module)
        self.assertEqual(got, {
            'shurelia': mock.sentinel.shurelia,
            'frelia': mock.sentinel.frelia,
            'tyria': mock.sentinel.tyria,
        })

    def test_load_filters_from_module_missing_all(self):
        """Test _load_filters_from_module() module missing __all__."""
        module = mock.NonCallableMock([])
        load = frelia.jinja._load_filters_from_module
        with self.assertRaises(AttributeError):
            load(module)

    def test_load_filters_from_module_missing_attribute(self):
        """Test _load_filters_from_module() module missing filter defined in __all__."""
        module = mock.NonCallableMock(['__all__'])
        module.__all__ = ['shurelia', 'frelia', 'tyria']
        load = frelia.jinja._load_filters_from_module
        with self.assertRaises(AttributeError):
            load(module)


class EnvironmentTestCase(unittest.TestCase):

    def test_default_option_values(self):
        """Test default option values."""
        env = frelia.jinja.Environment()
        self.assertEqual(env.trim_blocks, True)
        self.assertEqual(env.lstrip_blocks, True)
        self.assertEqual(env.auto_reload, False)

    def test_setting_custom_options(self):
        """Test setting custom options."""
        with self.subTest(trim_blocks=False):
            env = frelia.jinja.Environment(trim_blocks=False)
            self.assertEqual(env.trim_blocks, False)

        with self.subTest(trim_blocks=True):
            env = frelia.jinja.Environment(trim_blocks=True)
            self.assertEqual(env.trim_blocks, True)

    def test_customize_options(self):
        """Test _customize_options() classmethod."""
        patcher = mock.patch.dict(
            frelia.jinja.Environment._Environment__DEFAULT_OPTIONS,  # pylint: disable=no-member
            {'shurelia': 'bunnies'},
            clear=True)
        customize = frelia.jinja.Environment._customize_options

        with self.subTest('Replace option'):
            with patcher:
                got = customize({'shurelia': 'mir'})
            self.assertEqual(got, {'shurelia': 'mir'})

        with self.subTest('Add option'):
            with patcher:
                got = customize({'mir': 'bully'})
            self.assertEqual(got, {
                'shurelia': 'bunnies',
                'mir': 'bully',
            })

    def test_copy_default_options(self):
        """Test _copy_default_options() method."""
        default_options = frelia.jinja.Environment._Environment__DEFAULT_OPTIONS  # pylint: disable=no-member

        got = frelia.jinja.Environment._copy_default_options()
        self.assertEqual(got, default_options)
        self.assertIsNot(got, default_options)
