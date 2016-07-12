"""Transformations.

This module defines a generic Transformer, which groups object transformation
functions to be applied to objects.

This module also defines specific transformation callables, for example
transformations for document objects.

Document objects have metadata and content attributes.

"""


class Transformer:

    """Groups transformation functions."""

    def __init__(self, transformers=()):
        self.transformers = list(transformers)

    def transform(self, obj):
        """Apply all transformation functions to obj."""
        for func in self.transformers:
            func(obj)


class RenderJinja:

    """Document transformation callable that renders document content."""

    def __init__(self, env):
        self.env = env

    def __call__(self, document):
        content_as_template = self.env.from_string(document.content)
        rendered_content = content_as_template.render()
        document.content = rendered_content
