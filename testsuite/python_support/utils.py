import os
import os.path
import re
import shlex
import subprocess


LAL_ROOTDIR = os.path.abspath(os.environ['LIBADALANG_ROOTDIR'])
LAL_DISABLE_SHARED = bool(int(os.environ['LIBADALANG_DISABLE_SHARED']))
LAL_BUILD_MODE = os.environ['LIBADALANG_BUILD_MODE'] or "dev"

DIRECTORY_MISSING_RE = re.compile(
    r'.*\.gpr:\d+:\d+: warning:'
    r' \w+ directory ".*" (not found|does not exist)'
)


# Arguments to pass to GPR tools in order to process project files involving
# libadalang.gpr/langkit_support.gpr.
LIBRARY_KIND = 'static' if LAL_DISABLE_SHARED else 'relocatable'
GPR_ARGS = [
    '-XLIBRARY_TYPE={}'.format(LIBRARY_KIND),
    '-XXMLADA_BUILD={}'.format(LIBRARY_KIND),
    '-XBUILD_MODE={}'.format(LAL_BUILD_MODE),

    # Make sure GPRbuild does not try to rebuild Libadalang, as this will break
    # other tests running in parallel.
    '-XLIBADALANG_EXTERNALLY_BUILT=true',
]


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
    subprocess.check_call(
        ['gprbuild', '-P', project_file, '-q', '-p'] + GPR_ARGS
    )


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
                         stderr=subprocess.STDOUT,
                         encoding='ascii')
    stdout, _ = p.communicate()

    if p.returncode:
        print('nameres exitted with status code {}'.format(p.returncode))
        print('Command line was:', shlex.join(argv))
        print('Output was:')
        print('')
        print(stdout)
        return

    for line in stdout.splitlines():
        if not DIRECTORY_MISSING_RE.match(line.strip()):
            print(line)
