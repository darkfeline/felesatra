import logging
import os
import urllib.parse

from frelia.document import enja
import frelia.document.renderers as document_renderers
import frelia.fs
import frelia.page
from frelia import sitemap
import frelia.transforms.document as document_transforms
import frelia.transforms.generic as generic_transforms
import frelia.transforms.page as page_transforms
import yaml

import felesatra.transforms

logger = logging.getLogger(__name__)


def load_globals_dict(filename):
    """Load globals dict from YAML file."""
    with open(filename) as file:
        return yaml.load(file, Loader=yaml.CLoader)


def link_static_files(src, dst):
    frelia.fs.link_files(src, dst)


def load_pages(page_dir):
    page_loader = frelia.page.PageLoader(enja.read)
    return list(page_loader.load_pages(page_dir))


class BuildProcess:

    def __init__(self, build_dir, make_env):
        self.build_dir = build_dir
        self.make_env = make_env

    def __call__(self, globals_dict, pages):
        globals_dict = globals_dict.copy()

        logger.info('Preprocessing pages...')
        self._preprocess_pages(pages)

        logger.info('Making sitemap...')
        self._make_sitemap(globals_dict['site']['url'], pages)

        aggregation_pages, content_pages = self._partition_aggregation(pages)

        logger.info('Processing pages...')
        self._transform_template_pages(globals_dict, content_pages)

        globals_dict['site']['pages'] = content_pages

        logger.info('Processing aggregation pages...')
        env = self.make_env(globals_dict)
        self._transform_jinja_pages(env, aggregation_pages)

        self._render_pages(env, pages)
        self._write_pages(pages)

    _preprocess_pages = generic_transforms.ComposeTransforms([
        page_transforms.strip_page_extension,
        page_transforms.DateFromPath('published'),
        page_transforms.DocumentPageTransforms([
            felesatra.transforms.mark_aggregations,
            document_transforms.CopyMetadata('published', 'updated'),
            document_transforms.CopyMetadata('aggregate', 'index'),
            document_transforms.CopyMetadata('index', 'aggregate'),
            document_transforms.SetDefaultMetadata({
                'aggregate': True,
                'index': True,
            }),
        ]),
    ])

    def _make_sitemap(self, site_url, pages):
        output = sitemap.render(self._generate_urls(site_url, pages))
        with open(os.path.join(self.build_dir, 'sitemap.xml'), 'w') as file:
            file.write(output)

    @staticmethod
    def _generate_urls(site_url, pages):
        urljoin = urllib.parse.urljoin
        URL = frelia.sitemap.URL
        for page in pages:
            metadata = page.document.metadata
            if metadata.get('index', True):
                yield URL(
                    loc=urljoin(site_url, page.path),
                    lastmod=metadata['updated'])

    @staticmethod
    def _partition_aggregation(pages):
        aggregation_pages = []
        content_pages = []
        for page in pages:
            if page.document.metadata.get('aggregation', False):
                aggregation_pages.append(page)
            else:
                content_pages.append(page)
        return aggregation_pages, content_pages

    @staticmethod
    def _transform_template_pages(mapping, pages):
        transform = page_transforms.DocumentPageTransforms([
            document_transforms.RenderTemplate(mapping),
        ])
        transform(pages)

    @staticmethod
    def _transform_jinja_pages(env, pages):
        transform = page_transforms.DocumentPageTransforms([
            document_transforms.RenderJinja(env),
        ])
        transform(pages)

    @staticmethod
    def _render_pages(env, pages):
        document_renderer = document_renderers.JinjaDocumentRenderer(env)
        page_renderer = frelia.page.PageRenderer(document_renderer)
        page_renderer(pages)

    def _write_pages(self, pages):
        writer = frelia.page.PageWriter(self.build_dir)
        writer(pages)


class EnvironmentMaker:

    def __init__(self, env_class, **kwargs):
        self.env_class = env_class
        self.kwargs = kwargs

    def __call__(self, globals_dict=None):
        env = self.env_class(**self.kwargs)
        if globals_dict is not None:
            env.globals = globals_dict
        return env
