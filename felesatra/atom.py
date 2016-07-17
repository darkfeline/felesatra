import datetime
import urllib.parse

import frelia.atom


def render(site_url, path, pages):
    feed = frelia.atom.Feed(
        id=site_url,
        title='Feles Atra',
        updated=datetime.datetime.now(datetime.timezone.utc),
        rights='Copyright 2010-2016 Allen Li',
        links=[
            frelia.atom.Link(
                href=urllib.parse.urljoin(site_url, path),
                rel='self',
                type='application/atom+xml'),
        ],
        authors=[
            frelia.atom.Author(
                name='Allen Li',
                uri=site_url),
        ],
        entries=list(_generate_entries(site_url, pages)))
    return frelia.atom.render(feed)


def _generate_entries(site_url, pages):
    urljoin = urllib.parse.urljoin
    Entry = frelia.atom.Entry
    Link = frelia.atom.Link
    for page in pages:
        metadata = page.document.metadata
        if metadata.get('aggregate', True):
            url = urljoin(site_url, page.path)
            yield Entry(
                id=url,
                title=metadata['title'],
                updated=metadata['updated'],
                published=metadata['published'],
                summary=page.document.content,
                links=[
                    Link(
                        href=url,
                        rel='alternate',
                        type='text/html'),
                ])
