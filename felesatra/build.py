import logging
import os

from mir.frelia.document import enja
import mir.frelia.document.renderers as document_renderers
import mir.frelia.fs as fs_mod
import mir.frelia.page
import mir.frelia.transforms.document as document_transforms
import mir.frelia.transforms.generic as generic_transforms
import mir.frelia.transforms.page as page_transforms
import yaml

from felesatra import atom
from felesatra import sitemap
import felesatra.transforms

logger = logging.getLogger(__name__)


def build(conf):
    link_static_files(conf['static_dir'], conf[''])
    logger.info('Linking static files...')

    logger.info('Loading pages...')
    pages = load_pages(args.page_dir)
    rebase_paths = page_transforms.RebasePagePath(args.page_dir)
    rebase_paths(pages)

    logger.info('Building site...')
    builder = build.BuildProcess(
        args.build_dir,
        make_env,
        globals_dict,
        pages)
    builder()


def link_static_files(srcdir, dstdir):
    logger.info('Linking static files...')
    fs_mod.link_files(srcdir, dstdir)


def load_pages(pagedir):
    ...


class BuildProcess:

    def __init__(self, args):
        self.args = args

    def __init__(self, build_dir, make_env, globals_dict, pages):
        self.build_dir = build_dir
        self.make_env = make_env
        self.globals_dict = globals_dict.copy()
        self.pages = pages

    def __call__(self):
        self.globals_dict['site']['pages'] = self.pages

        logger.info('Preprocessing pages...')
        self._preprocess_pages(self.pages)

        logger.info('Making sitemap...')
        self._make_sitemap()

        aggregation_pages, content_pages = self._partition(self.pages)

        logger.info('Processing pages...')
        self._transform_template_pages(self.globals_dict, content_pages)

        self.globals_dict['site']['content_pages'] = content_pages

        logger.info('Processing aggregation pages...')
        env = self._make_env()
        self._transform_jinja_pages(env, aggregation_pages)

        logger.info('Making Atom feed...')
        self._make_atom()

        logger.info('Rendering pages...')
        self._render_pages(env)
        self._write_pages()

    @property
    def site_url(self):
        return self.globals_dict['site']['url']

    def _make_env(self):
        return self.make_env(self.globals_dict)

    _preprocess_pages = generic_transforms.ComposeTransforms([
        page_transforms.StripExtensions(),
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

    def _make_sitemap(self):
        output = sitemap.render(self.site_url, self.pages)
        with open(os.path.join(self.build_dir, 'sitemap.xml'), 'w') as file:
            file.write(output)

    def _make_atom(self):
        output = atom.render(self.site_url, 'atom.xml', self.pages)
        with open(os.path.join(self.build_dir, 'atom.xml'), 'w') as file:
            file.write(output)

    @staticmethod
    def _partition(pages):
        """Partition pages into aggregation pages and not."""
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

    def _render_pages(self, env):
        document_renderer = document_renderers.JinjaDocumentRenderer(env)
        page_renderer = frelia.page.PageRenderer(document_renderer)
        page_renderer(self.pages)

    def _write_pages(self):
        writer = frelia.page.PageWriter(self.build_dir)
        writer(self.pages)


class EnvironmentMaker:

    def __init__(self, env_class, **kwargs):
        self.env_class = env_class
        self.kwargs = kwargs

    def __call__(self, globals_dict=None):
        env = self.env_class(**self.kwargs)
        if globals_dict is not None:
            env.globals = globals_dict
        return env
