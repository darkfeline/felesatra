import os

from .fs import walk_files


def copy_static_files(src, dst):
    for src_path in walk_files(src):
        rel_path = os.path.relpath(src_path, src)
        dst_path = os.path.join(dst, rel_path)
        os.makedirs(os.path.dirname(dst_path), exist_ok=True)
        os.link(src_path, dst_path)
