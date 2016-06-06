"""Functions for "building" static files."""
import os

import frelia.fs


def link_static_files(src, dst):
    for src_path in frelia.fs.walk_files(src):
        rel_path = os.path.relpath(src_path, src)
        dst_path = os.path.join(dst, rel_path)
        os.makedirs(os.path.dirname(dst_path), exist_ok=True)
        os.link(src_path, dst_path)
