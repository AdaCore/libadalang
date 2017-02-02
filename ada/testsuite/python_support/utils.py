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


def gprbuild(project_file):
    """
    Invoke gprbuild on the given project file.

    This passes all the command-line arguments that are required to build a
    project that depends on Libadalang.
    """
    argv = ['gprbuild', '-P', project_file, '-q', '-p', '-m']
    argv.append('-XLIBRARY_TYPE={}'.format(
        'static' if LAL_DISABLE_SHARED else 'relocatable'
    ))
    subprocess.check_call(argv)
