from __future__ import annotations

import argparse
from dataclasses import dataclass
import json
import logging
from pathlib import Path
import re
import subprocess


@dataclass
class Module:
    name: str
    repo: str


MODULES = [
    Module(name="go.felesatra.moe/danbooru", repo="https://github.com/darkfeline/danbooru-go"),
    Module(name='go.felesatra.moe/anidb', repo='https://github.com/darkfeline/anidb-go'),
    Module(name='go.felesatra.moe/animanager', repo='https://github.com/darkfeline/animanager-go'),
    Module(name='go.felesatra.moe/binpack', repo='https://github.com/darkfeline/binpack'),
    Module(name='go.felesatra.moe/booru/dl', repo='https://github.com/darkfeline/booru-dl-go'),
    Module(name='go.felesatra.moe/danbooru', repo='https://goproxy.felesatra.moe/'),
    Module(name='go.felesatra.moe/dlsite', repo='https://github.com/darkfeline/dlsite-go'),
    Module(name='go.felesatra.moe/felesatra', repo='https://github.com/darkfeline/felesatra'),
    Module(name='go.felesatra.moe/go2/errors', repo='https://github.com/darkfeline/go2-errors'),
    Module(name='go.felesatra.moe/orbis', repo='https://github.com/darkfeline/orbis-go'),
    Module(name='go.felesatra.moe/pwnck', repo='https://github.com/darkfeline/pwnck'),
    Module(name='go.felesatra.moe/qualia', repo='https://github.com/darkfeline/qualia-go'),
    Module(name='go.felesatra.moe/sitemap', repo='https://github.com/darkfeline/sitemap-go'),
    Module(name='go.felesatra.moe/subcommands', repo='https://github.com/darkfeline/subcommands-go'),
    Module(name='go.felesatra.moe/xdg', repo='https://github.com/darkfeline/xdg-go'),
]


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('workdir', type=Path)
    parser.add_argument('outdir', type=Path)
    args = parser.parse_args()
    args.workdir.mkdir(parents=True, exist_ok=True)
    args.outdir.mkdir(parents=True, exist_ok=True)
    for module in MODULES:
        logging.info(f'Building module {module.name}')
        build_module(args.workdir, args.outdir, module)


def build_module(workdir: Path, outdir: Path, module: Module):
    checkout = Path(workdir, module.name)
    if checkout.exists():
        subprocess.run(['git', '-C', checkout, 'fetch'], check=True)
    else:
        checkout.parent.mkdir(parents=True, exist_ok=True)
        subprocess.run(['git', 'clone', module.repo, checkout], check=True)
    versions = get_versions(checkout)
    logging.info(f'Got versions for module {module.name}: {versions}')
    base_path(outdir, module.name).mkdir(parents=True, exist_ok=True)
    good_versions = []
    for version in versions:
        logging.info(f'Building module {module.name} version {version}')
        try:
            build_module_version(checkout, outdir, module.name, version)
        except Exception as e:
            logging.warning(f'Skipping module {module.name} version {version}: {e}')
        else:
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
def get_versions(checkout: Path) -> List[str]:
    p = subprocess.run(['git', '-C', checkout, 'tag'], capture_output=True, check=True)
    tags = p.stdout.decode().splitlines()
    return [tag for tag in tags if re.match(r'v[0-9.]+', tag)]


def get_commit_time(checkout: Path, ref: str) -> str:
    p = subprocess.run(['git', '-C', checkout, 'show', '-s', '--format=%cI', ref],
                       capture_output=True, check=True)
    return p.stdout.decode().strip()


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
