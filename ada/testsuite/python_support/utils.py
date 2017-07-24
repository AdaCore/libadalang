from __future__ import absolute_import, division, print_function

import os
import os.path
import subprocess


LAL_ROOTDIR = os.path.abspath(os.environ['LIBADALANG_ROOTDIR'])
LAL_DISABLE_SHARED = bool(int(os.environ['LIBADALANG_DISABLE_SHARED']))


def in_contrib(*args):
    """
    Return a path under the "contrib" subdir in the top of the repository.
    """
    return os.path.join(LAL_ROOTDIR, 'contrib', *args)


def get_ext_src(repo):
    """
    Return the absolute path to the external sources for repository "repo".

    :param str repo: Name of the source repository.
    :rtype: str
    """
    path = os.path.join(LAL_ROOTDIR, 'ada', 'testsuite', 'ext_src', repo)
    assert os.path.isdir(path)
    return path


def gprbuild(project_file):
    """
    Invoke gprbuild on the given project file.

    This passes all the command-line arguments that are required to build a
    project that depends on Libadalang.
    """
    argv = ['gprbuild', '-P', project_file, '-q', '-p', '-m']
    library_kind = 'static' if LAL_DISABLE_SHARED else 'relocatable'
    argv.extend([
        '-XLIBRARY_TYPE={}'.format(library_kind),
        '-XXMLADA_BUILD={}'.format(library_kind),
    ])
    subprocess.check_call(argv)
