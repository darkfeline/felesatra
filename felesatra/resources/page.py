"""Web page resource."""

import xml.etree.ElementTree as ET

from .base import FileResource
from felesatra.utils import tostring, findpop


class Webpage(FileResource):

    """Web page resource for rendering."""

    def __init__(self, path):
        super().__init__(path)
        root = ET.parse(self.path).getroot()

        meta = findpop(root, 'x-meta')
        self.meta = {
            'template': 'base.html',
            'title': '',
        }
        for child in meta:
            self.meta[child.tag.lstrip('x-')] = child.text

        self.content = ''.join(tostring(element) for element in root)

    def render(self, env, target):
        """Render this resource into target."""

        # Render page content itself first (e.g., macros).
        content_template = env.from_string(self.content)
        content = content_template.render(self.meta['template'])

        # Render page content into template.
        template = env.get_template(self.meta)
        context = {
            'title': self.meta['title'],
            'content': content,
        }
        with open(target, 'w') as file:
            file.write(template.render(context))
