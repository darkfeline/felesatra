"""Web page resource."""

import xml.etree.ElementTree as ET

from .base import FileResource
from felesatra.utils import tostring


class Webpage(FileResource):

    """Web page resource for rendering."""

    def render(self, env, target):
        """Render this resource into target."""
        content = ET.parse(self.path).getroot()
        title = content.find('title')
        content.remove(title)

        template = env.get_template('base.html')
        context = {
            'title': title.text,
            'content': ''.join(tostring(element) for element in content),
        }
        with open(target, 'w') as file:
            file.write(template.render(context))
