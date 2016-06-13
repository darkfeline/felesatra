"""Use enja to generate HTML."""

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
        return ('chibi-scheme', '-I{}'.format(self.module_path), '-Renja.main')


class EnjaFutures:

    def __init__(self, enja_command):
        self.enja_command = enja_command
        self.futures = []

    def __await__(self):
        return asyncio.gather(*self.futures)

    @frelia.descriptors.CachedProperty
    def command(self):
        return self.enja_command.command

    def create_enja_task(self, src, dst):
        coro = self.convert_enja_file(src, dst)
        future = asyncio.ensure_future(coro)
        self.futures.append(future)
        return future

    async def convert_enja_file(self, src, dst):
        with open(src, 'rb') as srcfile, open(dst, 'wb') as dstfile:
            proc = await asyncio.create_subprocess_exec(
                *self.command, stdin=srcfile, stdout=dstfile)
            await proc.wait()


def main():
    logging.basicConfig(level='INFO')
    args = parse_args()
    convert_enja_files(args.build_dir)


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('build_dir')
    return parser.parse_args()


def convert_enja_files(build_dir):
    loop = asyncio.get_event_loop()
    enja_futures = EnjaFutures(ChibiEnja('enja'))
    for src, dst in get_files_to_convert(build_dir):
        logger.info('Converting %s', src)
        enja_futures.create_enja_task(src, dst)
    loop.run_until_complete(enja_futures)
    loop.close()
    logger.info('Done.')


def get_files_to_convert(build_dir):
    for src in find_enja_files(build_dir):
        dst = os.path.splitext(src)[0] + '.html'
        yield src, dst


def find_enja_files(build_dir):
    yield from frelia.fs.filter_ext(frelia.fs.walk_files(build_dir), '.lisp')

if __name__ == '__main__':
    main()
