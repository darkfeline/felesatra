"""Blog resources."""

import datetime
import os

from .page import Webpage


class Blogpage(Webpage):

    """Web page extended with blog support.

    For now, all this does is extend Webpage with automagic published date detection.

    """

    def __init__(self, path):
        super().__init__(path)

        if 'published' not in self.meta:
            path = os.path.dirname(path)
            path, day = os.path.split(path)
            path, month = os.path.split(path)
            path, year = os.path.split(path)
            self.meta['published'] = datetime.datetime(int(year), int(month), int(day))
