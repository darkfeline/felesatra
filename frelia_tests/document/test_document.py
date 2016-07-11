from unittest import mock

import frelia.document


def test_documents_equal():
    doc1 = frelia.document.Document(
        mock.sentinel.loader1,
        {'ion': 'nero'},
        'content')
    doc2 = frelia.document.Document(
        mock.sentinel.loader2,
        {'ion': 'nero'},
        'content')
    assert doc1 == doc2


def test_document_metadata_not_equal():
    doc1 = frelia.document.Document(
        mock.sentinel.loader1,
        {'ion': 'nero'},
        'content')
    doc2 = frelia.document.Document(
        mock.sentinel.loader2,
        {'ion': 'nay'},
        'content')
    assert doc1 != doc2


def test_document_content_not_equal():
    doc1 = frelia.document.Document(
        mock.sentinel.loader1,
        {'ion': 'nay'},
        'content')
    doc2 = frelia.document.Document(
        mock.sentinel.loader2,
        {'ion': 'nay'},
        'no content')
    assert doc1 != doc2


def test_compare_document_to_nondocument():
    doc = frelia.document.Document(
        mock.sentinel.loader,
        {'ion': 'nero'},
        'content')
    assert doc != 1
