"""enja is frelia's document format built on HTML."""

import bs4
import dateutil.parser


class _MetadataLoader:

    def __init__(self, soup):
        self.head = soup.head

    def _find(self, tag):
        try:
            return self.head.find(tag).text
        except AttributeError:
            return None

    @property
    def title(self):
        return self._find('title')

    @property
    def template(self):
        return self._find('enja-template')

    @property
    def published(self):
        text = self._find('enja-published')
        if text is None:
            return None
        else:
            return dateutil.parser.parse(text)

    @property
    def updated(self):
        text = self._find('enja-updated')
        if text is None:
            return None
        else:
            return dateutil.parser.parse(text)

    @property
    def metadata(self):
        return {
            'title': self.title,
            'template': self.template,
            'published': self.published,
            'updated': self.updated,
        }


class EnjaDocument:

    _metadata_loader = _MetadataLoader

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
        return cls.from_string(file)

    @classmethod
    def from_string(cls, s):
        soup = bs4.BeautifulSoup(s, 'lxml')
        metadata = cls._get_metadata(soup)
        content = cls._get_content(soup)
        return cls(metadata, content)

    @classmethod
    def _get_metadata(cls, soup):
        """Get document metadata."""
        return cls._metadata_loader(soup).metadata

    @staticmethod
    def _get_content(soup):
        if soup.body is None:
            return ''
        else:
            return ''.join(str(tag) for tag in soup.body.children)
