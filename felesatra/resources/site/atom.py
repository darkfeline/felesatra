"""Atom feed rendering resources."""

import datetime
import logging
from collections import namedtuple
from datetime import timezone

from felesatra.resources.abc import Resource

logger = logging.getLogger(__name__)

Author = namedtuple(
    'Author',
    ['name', 'uri', 'email'])

Entry = namedtuple(
    'Entry',
    ['id', 'title', 'updated', 'links', 'summary', 'published'])

Link = namedtuple(
    'Link',
    ['href', 'rel', 'type'])

Category = namedtuple(
    'Category',
    ['term', 'scheme', 'label'])


class AtomResource(Resource):

    """Atom feed resource."""

    def __init__(self, context):
        self.context = context

    def __repr__(self):
        return "AtomResource({})".format(self.context)

    def walk(self, env):
        pass

    def render(self, env, target):
        """Render this resource into target."""
        super().render(env, target)
        context = dict(self.context)
        entries = env.globals['page_index']
        entries = [entry.atom_entry() for entry in entries]
        logger.debug('Atom %r', entries)
        context['entries'] = entries
        if entries:
            updated = max(entry.updated for entry in entries)
        else:
            updated = datetime.datetime.now(timezone.utc)
        context['updated'] = updated
        template = env.get_template('atom.xml')
        content = template.render(context)
        with open(target, 'w') as file:
            file.write(content)
