"""Transformations.

This module defines a generic TransformGroup, which groups object
transformation functions to be applied to objects.

This module also defines specific transformation callables, for example
transformations for document objects.

Document objects have metadata and content attributes.

"""


class TransformGroup:

    """Groups transformation functions."""

    def __init__(self, transforms=()):
        self.transforms = list(transforms)

    def __call__(self, obj):
        """Apply all transformation functions to obj."""
        for transform in self.transforms:
            transform(obj)


class DocumentPageTransform:

    """Maps a document transformation to a page transformation."""

    def __init__(self, transform):
        self.transform = transform

    def __call__(self, page):
        self.transform(page.document)


class RenderJinja:

    """Document transformation callable that renders document content."""

    def __init__(self, env):
        self.env = env

    def __call__(self, document):
        content_as_template = self.env.from_string(document.content)
        rendered_content = content_as_template.render()
        document.content = rendered_content
