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
    enja_command = ChibiEnja('enja')
    futures = []
    for src, dst in get_files_to_convert(build_dir):
        logger.info('Converting %s', src)
        coro = convert_enja(enja_command, src, dst)
        future = asyncio.ensure_future(coro)
        futures.append(future)
    loop.run_until_complete(asyncio.gather(*futures))
    loop.close()
    logger.info('Done.')


def get_files_to_convert(build_dir):
    for src in find_enja_files(build_dir):
        dst = os.path.splitext(src)[0] + '.html'
        yield src, dst


def find_enja_files(build_dir):
    yield from frelia.fs.filter_ext(frelia.fs.walk_files(build_dir), '.lisp')


async def convert_enja(enja_command, src, dst):
    with open(src, 'rb') as srcfile, open(dst, 'wb') as dstfile:
        proc = await asyncio.create_subprocess_exec(
            *enja_command.command,
            stdin=srcfile, stdout=dstfile)
        await proc.wait()

if __name__ == '__main__':
    main()
