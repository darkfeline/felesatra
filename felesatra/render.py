"""Website rendering."""


def render(res, dest_dir):
    """Render resources."""
    # env = make_environment()
    for dirpath, dirnames, filenames in os.walk(src_dir):
        dirpath = dirpath[len(src_dir):]
        for dirname in dirnames:
            os.makedirs(
                os.path.join(dest_dir, dirpath, dirname),
                exist_ok=True)
        for filename in filenames:
            if filename.endswith('.html'):
                pass
            else:
                shutil.copy(
                    os.path.join(src_dir, dirpath, filename),
                    os.path.join(dest_dir, dirpath, filename))
