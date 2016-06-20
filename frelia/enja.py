"""enja is frelia's document format built on HTML."""

from bs4 import BeautifulSoup


class EnjaDocument:

    def __init__(self, metadata, content):
        self.metadata = metadata
        self.content = content

    @classmethod
    def fromfile(cls, file):
        return cls.fromstring(file)

    @classmethod
    def fromstring(cls, s):
        soup = BeautifulSoup(s, 'lxml')
