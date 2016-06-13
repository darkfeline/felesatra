"""Call enja to generate HTML."""

import abc
import argparse
import asyncio
import os
import logging

import frelia.fs
import frelia.descriptors

logger = logging.getLogger(__name__)


class EnjaCommand(abc.ABC):

    @property
    @abc.abstractmethod
    def command(self):
        raise NotImplementedError


class ChibiEnja(EnjaCommand):

    def __init__(self, module_path):
        self.module_path = module_path

    @frelia.descriptors.CachedProperty
    def command(self):
        return ['chibi-scheme', '-I{}'.format(self.module_path), '-Renja.main']


async def convert_enja_dirtree(enja_command, dirpath):
    futures = []
    for src in filter_lisp(frelia.fs.walk_files(dirpath)):
        logger.info('Converting %s', src)
        dst = os.path.splitext(src)[0] + '.html'
        future = asyncio.ensure_future(convert_enja(enja_command, src, dst))
        futures.append(future)
    await asyncio.gather(*futures)
    logger.info('Done.')


def filter_lisp(filenames):
    yield from (x for x in filenames if x.endswith('.lisp'))


async def convert_enja(enja_command, src, dst):
    with open(src, 'rb') as srcfile, open(dst, 'wb') as dstfile:
        proc = await asyncio.create_subprocess_exec(
            *enja_command.command,
            stdin=srcfile, stdout=dstfile)
        await proc.wait()


def main():
    logging.basicConfig(level='INFO')
    args = parse_args()
    loop = asyncio.get_event_loop()
    coro = convert_enja_dirtree(ChibiEnja('enja'), args.build_dir)
    loop.run_until_complete(coro)
    loop.close()


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('build_dir')
    return parser.parse_args()

if __name__ == '__main__':
    main()
