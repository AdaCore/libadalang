from __future__ import absolute_import, division, print_function

import os
import os.path
import pipes
import re
import subprocess


LAL_ROOTDIR = os.path.abspath(os.environ['LIBADALANG_ROOTDIR'])
LAL_DISABLE_SHARED = bool(int(os.environ['LIBADALANG_DISABLE_SHARED']))

DIRECTORY_MISSING_RE = re.compile(
    r'.*\.gpr:\d+:\d+: warning:'
    r' \w+ directory ".*" (not found|does not exist)'
)


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


def get_gnatcoll_project_file():
    """
    Return the absolute path to GNATCOLL's project file.

    For convenience in infrastructure-based testsuite runs, we make it possible
    for GNATCOLL sources to be outside of the testsuite tree: in this case, we
    expect the infrastructure to specify where the "gnatcoll.gpr" project file
    lies.

    :rtype: str
    """
    gnatcoll_dir = get_ext_src('gnatcoll')
    fn = os.path.join(gnatcoll_dir, 'project_file_path.txt')
    if os.path.exists(fn):
        with open(fn) as f:
            return f.read().strip()
    else:
        return os.path.join(gnatcoll_dir, 'src', 'gnatcoll.gpr')


def gprbuild(project_file):
    """
    Invoke gprbuild on the given project file.

    This passes all the command-line arguments that are required to build a
    project that depends on Libadalang.
    """
    argv = ['gprbuild', '-P', project_file, '-q', '-p']
    library_kind = 'static' if LAL_DISABLE_SHARED else 'relocatable'
    argv.extend([
        '-XLIBRARY_TYPE={}'.format(library_kind),
        '-XXMLADA_BUILD={}'.format(library_kind),
    ])
    subprocess.check_call(argv)


def run_nameres(args):
    """
    Run the name resolution program with the given arguments.

    If it exits with a non-zero status code, print an error message, display
    its output and stop.  Otherwise, display its output with warnings about
    missing directories filtered out.

    :param list[str] args: Arguments to pass to nameres.
    """
    argv = ['nameres'] + args
    p = subprocess.Popen(argv, stdout=subprocess.PIPE,
                         stderr=subprocess.STDOUT)
    stdout, _ = p.communicate()

    if p.returncode:
        print('nameres exitted with status code {}'.format(p.returncode))
        print('Command line was:', ' '.join(pipes.quote(a) for a in argv))
        print('Output was:')
        print('')
        print(stdout)
        return

    for line in stdout.splitlines():
        line = line.strip()
        if not DIRECTORY_MISSING_RE.match(line):
            print(line)
