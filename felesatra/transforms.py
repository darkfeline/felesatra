def mark_aggregations(documents):
    """Don't aggregate aggregation pages."""
    for document in documents:
        metadata = document.metadata
        if metadata.get('aggregation', False):
            metadata['aggregate'] = False
