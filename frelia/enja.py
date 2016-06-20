"""enja is frelia's document format built on HTML."""

from bs4 import BeautifulSoup


class EnjaDocument:

    def __init__(self, metadata, content):
        self.metadata = metadata
        self.content = content

    def __repr__(self):
        return '{classname}({metadata!r}, {content!r})'.format(
            classname=type(self).__name__,
            metadata=self.metadata,
            content=self.content)

    @classmethod
    def from_file(cls, file):
        return cls.fromstring(file)

    @classmethod
    def from_string(cls, s):
        soup = BeautifulSoup(s, 'lxml')
        metadata = cls._get_metadata(soup)
        content = cls._get_content(soup)
        return cls(metadata, content)

    @staticmethod
    def _get_metadata(soup):
        if soup.head is None:
            return {}
        else:
            return {
                tag['name']: tag['content']
                for tag in soup.head('meta')
                if 'name' in tag.attrs
            }

    @staticmethod
    def _get_content(soup):
        if soup.body is None:
            return ''
        else:
            return ''.join(str(tag) for tag in soup.body.children)
