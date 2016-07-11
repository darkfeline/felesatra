"""Atom feed rendering resources."""

import datetime
import logging
from collections import namedtuple
from datetime import timezone

from felesatra.fields import AttrField, DateTimeField, ListField, StringField
from felesatra.resources.abc import Resource

logger = logging.getLogger(__name__)

# NOTE: The distinction between using namedtuple and classes with Fields for
# Atom constructs is whether it will be rendered as a single tag with
# attributes or a tag with nested tag fields.  See the atom.xml template and
# the Atom spec for details.

Author = namedtuple(
    'Author',
    ['name', 'uri', 'email'])


class Link:

    """Represents an Atom link."""

    __slots__ = ['__weakref__', 'href']

    def __init__(self, href):
        self.href = href

    rel = AttrField()
    type = AttrField()


class Category:

    """Represents an Atom category."""

    __slots__ = ['__weakref__', 'term']

    def __init__(self, term):
        self.term = term

    scheme = AttrField()
    label = AttrField()


class Entry:

    """Represents one Atom entry."""

    __slots__ = ['__weakref__', 'id', 'title', 'updated']

    def __init__(self, entry_id, title, updated):
        self.id = entry_id
        self.title = title
        self.updated = updated

    links = ListField()
    summary = StringField()
    published = DateTimeField()
    categories = ListField()

    def add_link(self, href, rel=None, link_type=None):
        """Add a link to this entry."""
        link = Link(href)
        if rel is not None:
            link.rel = rel
        if link_type is not None:
            link.type = link_type
        self.links.append(link)

    def add_category(self, term, scheme=None, label=None):
        """Add a category to this entry."""
        category = Category(term)
        if scheme is not None:
            category.scheme = scheme
        if label is not None:
            category.label = label
        self.categories.append(category)


class AtomResource(Resource):

    """Atom feed resource."""

    def __init__(self, context):
        self.context = context

    def __repr__(self):
        return "AtomResource({})".format(self.context)

    def render(self, env, target):
        """Render this resource into target."""
        super().render(env, target)
        # Make a copy of the Atom context.
        context = dict(self.context)
        # Set up Atom entries.
        entries = env.globals['page_index']
        entries = [entry.atom_entry() for entry in entries if entry.include_in_atom]
        context['entries'] = entries
        # Calculate updated time for Atom feed.
        if entries:
            updated = max(entry.updated for entry in entries)
        else:
            updated = datetime.datetime.now(timezone.utc)
        context['updated'] = updated
        # Render Atom feed.
        template = env.get_template('atom.xml')
        content = template.render(context)
        with open(target, 'w') as file:
            file.write(content)
