import os
import os.path


LAL_ROOTDIR = os.path.abspath(os.environ['LIBADALANG_ROOTDIR'])


def in_contrib(*args):
    """
    Return a path under the "contrib" subdir in the top of the repository.
    """
    return os.path.join(LAL_ROOTDIR, 'contrib', *args)
