import urllib.parse

import frelia.sitemap


def render(site_url, pages):
    urls = _generate_urls(site_url, pages)
    return frelia.sitemap.render(urls)


def _generate_urls(site_url, pages):
    urljoin = urllib.parse.urljoin
    URL = frelia.sitemap.URL
    for page in pages:
        metadata = page.document.metadata
        if metadata.get('index', True):
            yield URL(
                loc=urljoin(site_url, page.path),
                lastmod=metadata['updated'])
