from __future__ import annotations

import argparse
from dataclasses import dataclass
from functools import partial
import json
import logging
import multiprocessing
from pathlib import Path
import re
import subprocess
from subprocess import DEVNULL


@dataclass
class Module:
    name: str
    repo: str


MODULES = [
    Module(name='go.felesatra.moe/anidb', repo='https://github.com/darkfeline/anidb-go'),
    Module(name='go.felesatra.moe/animanager', repo='https://github.com/darkfeline/animanager-go'),
    Module(name='go.felesatra.moe/binpack', repo='https://github.com/darkfeline/binpack'),
    Module(name='go.felesatra.moe/danbooru', repo='https://github.com/darkfeline/danbooru-go'),
    Module(name='go.felesatra.moe/dlsite', repo='https://github.com/darkfeline/dlsite-go'),
    Module(name='go.felesatra.moe/dlsite/v2', repo='https://github.com/darkfeline/dlsite-go'),
    Module(name='go.felesatra.moe/dlsite-cmd', repo='https://github.com/darkfeline/dlsite-cmd-go'),
    Module(name='go.felesatra.moe/go2/errors', repo='https://github.com/darkfeline/go2-errors'),
    Module(name='go.felesatra.moe/keeper', repo='https://github.com/darkfeline/keeper-go'),
    Module(name='go.felesatra.moe/linelist', repo='https://github.com/darkfeline/go-linelist'),
    Module(name='go.felesatra.moe/pwnck', repo='https://github.com/darkfeline/pwnck'),
    Module(name='go.felesatra.moe/qualia', repo='https://github.com/darkfeline/qualia-go'),
    Module(name='go.felesatra.moe/saucenao', repo='https://github.com/darkfeline/saucenao'),
    Module(name='go.felesatra.moe/sitemap', repo='https://github.com/darkfeline/sitemap-go'),
    Module(name='go.felesatra.moe/xdg', repo='https://github.com/darkfeline/xdg-go'),
]


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('workdir', type=Path)
    parser.add_argument('outdir', type=Path)
    args = parser.parse_args()

    args.workdir.mkdir(parents=True, exist_ok=True)
    args.outdir.mkdir(parents=True, exist_ok=True)
    f = partial(build_module, args.workdir, args.outdir)
    with multiprocessing.Pool(processes=len(MODULES)) as pool:
        pool.map(f, MODULES)


def build_module(workdir: Path, outdir: Path, module: Module):
    checkout = Path(workdir, module.name)
    if checkout.exists():
        subprocess.run(['git', '-C', checkout, 'fetch'],
                       stdout=DEVNULL, stderr=DEVNULL, check=True)
    else:
        checkout.parent.mkdir(parents=True, exist_ok=True)
        subprocess.run(['git', 'clone', '--bare', module.repo, checkout],
                       stdout=DEVNULL, stderr=DEVNULL, check=True)
    versions = get_versions(checkout, module)
    logging.info(f'Got versions for module {module.name}: {versions}')
    base_path(outdir, module.name).mkdir(parents=True, exist_ok=True)
    good_versions = []
    for version in versions:
        if not has_mod_file(checkout, version):
            logging.warning(f'Module {module.name} version {version} missing mod file; skipping')
            continue
        logging.info(f'Building module {module.name} version {version}')
        build_module_version(checkout, outdir, module.name, version)
        good_versions.append(version)
    write_module_version_list(outdir, module.name, good_versions)


def write_module_version_list(outdir: Path, module_name: str, versions: Iterable[str]):
    list_path(outdir, module_name).write_text(
        ''.join(version + '\n' for version in versions))


def build_module_version(checkout: Path, outdir: Path, module_name: str, version: str):
    info = {
        'version': version,
        'time': get_commit_time(checkout, version),
    }
    info_path(outdir, module_name, version).write_text(json.dumps(info))
    mod_path(outdir, module_name, version).write_text(get_mod_file(checkout, version))
    with zip_path(outdir, module_name, version).open('wb') as f:
        subprocess.run(['git', '-C', checkout, 'archive', '--format=zip',
                        f'--prefix={module_name}@{version}/', version],
                       check=True, stdout=f)


# Basic Git operations
def get_versions(checkout: Path, module: Module) -> List[str]:
    p = subprocess.run(['git', '-C', checkout, 'tag'], capture_output=True, check=True)
    tags = p.stdout.decode().splitlines()
    return [tag for tag in tags if is_tag_for_module_version(module, tag)]


def is_tag_for_module_version(module: Module, tag: str) -> bool:
    m = re.search(r'/(v[0-9]+)$', module.name)
    if not m:
        return bool(re.match(r'v[01]\.', tag))
    version = m.group(1)
    return tag.startswith(version + '.')


def get_commit_time(checkout: Path, ref: str) -> str:
    p = subprocess.run(['git', '-C', checkout, 'show', '-s', '--format=%cI', f'{ref}^{{commit}}'],
                       capture_output=True, check=True)
    return p.stdout.decode().strip()


def has_mod_file(checkout: Path, ref: str) -> str:
    p = subprocess.run(['git', '-C', checkout, 'show', f'{ref}:go.mod'],
                       stdout=DEVNULL, stderr=DEVNULL)
    return p.returncode == 0


def get_mod_file(checkout: Path, ref: str) -> str:
    p = subprocess.run(['git', '-C', checkout, 'show', f'{ref}:go.mod'],
                       capture_output=True, check=True)
    return p.stdout.decode()


# Paths
def base_path(outdir: Path, module_name: str) -> Path:
    return outdir / module_name / '@v'


def list_path(outdir: Path, module_name: str) -> Path:
    return outdir / module_name / '@v' / 'list'


def info_path(outdir: Path, module_name: str, version: str) -> Path:
    return outdir / module_name / '@v' / f'{version}.info'


def mod_path(outdir: Path, module_name: str, version: str) -> Path:
    return outdir / module_name / '@v' / f'{version}.mod'


def zip_path(outdir: Path, module_name: str, version: str) -> Path:
    return outdir / module_name / '@v' / f'{version}.zip'


if __name__ == '__main__':
    main()
