from .fs import walk_files


def load_pages(page_dir):
    for filepath in walk_files(page_dir):
        pass


class Page:

    def __init__(self, metadata, content):
        self.metadata = metadata
        self.content = content
