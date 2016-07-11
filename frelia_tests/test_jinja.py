# pylint: disable=protected-access

import unittest
from unittest import mock

import frelia.jinja


class LoadFiltersTestCase(unittest.TestCase):

    def test_load_filters_from_module(self):
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


class EnvironmentTestCase(unittest.TestCase):

    def test_default_options(self):
        env = frelia.jinja.Environment()
        self.assertEqual(env.trim_blocks, True)
        self.assertEqual(env.lstrip_blocks, True)
        self.assertEqual(env.auto_reload, False)

    def test_custom_options(self):
        with self.subTest(trim_blocks=False):
            env = frelia.jinja.Environment(trim_blocks=False)
            self.assertEqual(env.trim_blocks, False)

        with self.subTest(trim_blocks=True):
            env = frelia.jinja.Environment(trim_blocks=True)
            self.assertEqual(env.trim_blocks, True)

    def test_load_filters_from_module_missing_all(self):
        module = mock.NonCallableMock([])
        with self.assertRaises(AttributeError):
            frelia.jinja.Environment._load_filters_from_module(module)

    def test_load_filters_from_module_missing_attribute(self):
        module = mock.NonCallableMock(['__all__'])
        module.__all__ = ['shurelia', 'frelia', 'tyria']
        with self.assertRaises(AttributeError):
            frelia.jinja.Environment._load_filters_from_module(module)

    def test_customize_options(self):
        patcher = mock.patch.dict(
            frelia.jinja.Environment._Environment__DEFAULT_OPTIONS,
            {'shurelia': 'bunnies'},
            clear=True)

        with self.subTest('Replace option'):
            with patcher:
                got = frelia.jinja.Environment._customize_options({'shurelia': 'mir'})
            self.assertEqual(got, {'shurelia': 'mir'})

        with self.subTest('Add option'):
            with patcher:
                got = frelia.jinja.Environment._customize_options({'mir': 'bully'})
            self.assertEqual(got, {
                'shurelia': 'bunnies',
                'mir': 'bully',
            })
