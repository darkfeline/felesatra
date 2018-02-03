"""Resource indexing."""

from collections import namedtuple

from felesatra.fields import AttrField
from felesatra.fields import BoolField
from felesatra.fields import DateTimeField
from felesatra.fields import ListField
from felesatra.fields import StringField


SitemapURL = namedtuple(
    'SitemapURL',
    ['loc', 'lastmod', 'changefreq', 'priority'])


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


class PageIndexEntry:

    """Page index entry used for many things.

    For example: sitemap, Atom feed, site index
    """

    __slots__ = ['__weakref__', 'href', 'title']

    def __init__(self, href, title):
        self.href = href
        self.title = title

    published = DateTimeField()
    updated = DateTimeField()
    category = StringField()
    tags = ListField()

    summary = StringField()

    include_in_sitemap = BoolField(True)
    include_in_atom = BoolField(True)

    def sitemap_entry(self):
        """Return sitemap entry of page index."""
        return SitemapURL(self.href, self.updated, None, None)

    def atom_entry(self):
        """Return Atom entry of page index."""
        entry = Entry(self.href, self.title, self.updated)
        entry.add_link(self.href, 'alternate', 'text/html')
        entry.summary = self.summary
        entry.published = self.published
        if self.category:
            entry.add_category(self.category, 'category')
        for tag in self.tags:
            entry.add_category(tag, 'tag')
        return entry
