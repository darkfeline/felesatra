import unittest

import frelia.jinja.env

class EnvironmentTestCase(unittest.TestCase):

    def test_environment_globals(self):
        env = frelia.jinja.env.Environment({'foo': 'bar'})
        self.assertEqual(env.globals, {'foo': 'bar'})
