def set_default_metadata(pages):
    """Set some default metadata on pages."""
    for page in pages:
        metadata = page.document.metadata
        metadata.setdefault('aggregate', True)
