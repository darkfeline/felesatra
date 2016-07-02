from collections import Counter
import unittest
from unittest import mock

import frelia.fs


class WalkFilesTestCase(unittest.TestCase):

    @mock.patch('os.walk')
    def test_walk_files(self, walk):
        walk.return_value = [
            ('foo', ['bar'], ['baz']),
            ('spam', ['eggs'], ['bacon']),
        ]

        got = Counter(frelia.fs.walk_files('path'))

        walk.assert_called_once_with('path')
        self.assertEqual(got, Counter(['foo/baz', 'spam/bacon']))


class LinkFilesTestCase(unittest.TestCase):

    @mock.patch('os.link')
    @mock.patch('os.makedirs')
    @mock.patch('frelia.fs.walk_files')
    def test_link_files(self, walk, makedirs, link):
        walk.return_value = [
            'src/foo/bar/baz',
            'src/spam/eggs/bacon',
        ]

        frelia.fs.link_files('src', 'dst')

        walk.assert_called_once_with('src')
        self.assertEqual(
            makedirs.mock_calls,
            [
                mock.call('dst/foo/bar', exist_ok=True),
                mock.call('dst/spam/eggs', exist_ok=True),
            ])
        self.assertEqual(
            link.mock_calls,
            [
                mock.call('src/foo/bar/baz','dst/foo/bar/baz'),
                mock.call('src/spam/eggs/bacon','dst/spam/eggs/bacon'),
            ])


class FilterExtTestCase(unittest.TestCase):

    def test_filter_ext(self):
        files = [
            'page.html',
            'page2.HTML',
            'script.js',
        ]
        self.assertEqual(
            list(frelia.fs.filter_ext(files, '.html')),
            ['page.html'])
