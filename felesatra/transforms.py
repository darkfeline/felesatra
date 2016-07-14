def mark_aggregations(documents):
    """Don't aggregate aggregation pages."""
    for document in documents:
        metadata = document.metadata
        if metadata.get('aggregation', False):
            metadata['aggregate'] = False


def set_updated_from_published(documents):
    """Set updated using published."""
    for document in documents:
        metadata = document.metadata
        if 'updated' not in metadata and 'published' in metadata:
            metadata['updated'] = metadata['published']
