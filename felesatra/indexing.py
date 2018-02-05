import csv
import datetime
from typing import NamedTuple


class Entry(NamedTuple):
    path: str
    title: str
    published: 'datetime.date'
    modified: 'datetime.date'


def dump(entries, file):
    writer = csv.writer(file, dialect='unix')
    writer.writerows(entries)


def load(file):
    reader = csv.reader(file, dialect='unix')
    for row in reader:
        entry = Entry._make(row)
        yield entry._replace(
            published=_parse_date(entry.published),
            modified=_parse_date(entry.modified),
        )


def _parse_date(string):
    return datetime.datetime.strptime(string, '%Y-%m-%d').date()
