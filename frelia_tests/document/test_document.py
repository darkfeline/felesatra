import frelia.document


def test_documents_equal():
    doc1 = frelia.document.Document(
        {'ion': 'nero'},
        'content')
    doc2 = frelia.document.Document(
        {'ion': 'nero'},
        'content')
    assert doc1 == doc2


def test_document_metadata_not_equal():
    doc1 = frelia.document.Document(
        {'ion': 'nero'},
        'content')
    doc2 = frelia.document.Document(
        {'ion': 'nay'},
        'content')
    assert doc1 != doc2


def test_document_content_not_equal():
    doc1 = frelia.document.Document(
        {'ion': 'nay'},
        'content')
    doc2 = frelia.document.Document(
        {'ion': 'nay'},
        'no content')
    assert doc1 != doc2


def test_compare_document_to_nondocument():
    doc = frelia.document.Document({'ion': 'nero'}, 'content')
    assert doc != 1
