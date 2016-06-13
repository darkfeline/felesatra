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
    if args.clean:
        pass
    else:
        convert_enja_files(args.build_dir)


def convert_enja_files(build_dir):
    loop = asyncio.get_event_loop()
    enja_command = ChibiEnja('enja')
    futures = []
    for src in filter_lisp(frelia.fs.walk_files(build_dir)):
        logger.info('Converting %s', src)
        dst = os.path.splitext(src)[0] + '.html'
        coro = convert_enja(enja_command, src, dst)
        future = asyncio.ensure_future(coro)
        futures.append(future)
    loop.run_until_complete(asyncio.gather(*futures))
    loop.close()
    logger.info('Done.')


def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('build_dir')
    parser.add_argument('--clean', action='store_true')
    return parser.parse_args()

if __name__ == '__main__':
    main()
