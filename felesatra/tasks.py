import logging

from frelia.document import enja
import frelia.fs
import frelia.jinja
from frelia import task
import jinja2
import yaml

from felesatra import build

logger = logging.getLogger(__name__)

_task = task.Task.decorate


@_task
def link_static_files(static_dir, build_dir):
    logger.info('Linking static files...')
    frelia.fs.link_files(static_dir, build_dir)


@_task
def env_maker(template_dir):
    return build.EnvironmentMaker(
        frelia.jinja.Environment,
        loader=jinja2.FileSystemLoader(template_dir),
        extensions=['jinja2.ext.with_'])


@_task
def globals_dict(globals_file, site_url):
    with open(globals_file) as file:
        globals_dict_ = yaml.load(file, Loader=yaml.CLoader)
    globals_dict_['site']['url'] = site_url
    return globals_dict_


@_task
def pages(page_dir):
    page_loader = frelia.page.PageLoader(enja.read)
    return list(page_loader.load_pages(page_dir))
