import argparse
import glob
import os.path
import pipes
import shutil
import subprocess
import sys
import inspect

from langkit.utils import Colors, printcol


class Directories(object):
    """
    Helper class used to get various path in source/build/install trees.
    """

    def __init__(self, lang_source_dir=None, build_dir=None, install_dir=None):
        self.root_lang_source_dir = lang_source_dir
        self.root_langkit_source_dir = os.path.dirname(
            os.path.abspath(__file__)
        )
        self.root_build_dir = build_dir
        self.root_install_dir = install_dir

    def set_build_dir(self, build_dir):
        self.root_build_dir = os.path.abspath(build_dir)

    def set_install_dir(self, install_dir):
        self.root_install_dir = os.path.abspath(install_dir)

    def lang_source_dir(self, *args):
        return os.path.join(self.root_lang_source_dir, *args)

    def build_dir(self, *args):
        return os.path.join(self.root_build_dir, *args)

    def install_dir(self, *args):
        return os.path.join(self.root_install_dir, *args)

    def langkit_source_dir(self, *args):
        """
        Build and return the path for ARGS under the root source directory for
        langkit.
        """
        return os.path.join(self.root_langkit_source_dir, *args)


class Coverage(object):
    """
    Guard object used to compute code coverage (optionally).
    """

    def __init__(self, dirs):
        self.dirs = dirs

        import coverage
        self.cov = coverage.coverage(
            branch=True,
            source=[
                self.dirs.langkit_source_dir(),
                self.dirs.lang_source_dir(),
            ],
            omit=[
                self.dirs.langkit_source_dir('libmanage.py'),
                self.dirs.lang_source_dir('manage.py'),
                self.dirs.lang_source_dir('env.py'),
            ],
        )

        self.cov.exclude('def __repr__')
        self.cov.exclude('raise NotImplementedError()')
        self.cov.exclude('assert False')

    def start(self):
        self.cov.start()

    def stop(self):
        self.cov.stop()

    def generate_report(self):
        self.cov.html_report(
            directory=self.dirs.build_dir('coverage'),
            ignore_errors=True
        )


def get_cpu_count():
    # The "multiprocessing" module is not available on GNATpython's
    # distribution for PPC AIX and the "cpu_count" is not available on Windows:
    # give up on default parallelism on these platforms.
    try:
        import multiprocessing
        return multiprocessing.cpu_count()
    except (ImportError, NotImplementedError):
        return 1


class ManageScript(object):

    BUILD_MODES = ('dev', 'prod')

    def __init__(self):

        self.dirs = Directories(
            # It is assumed that manage.py is at the root of the language
            # definition source directory.
            lang_source_dir=os.path.dirname(
                os.path.abspath(inspect.getfile(self.__class__))
            )
        )

        ########################
        # Main argument parser #
        ########################

        self.args_parser = args_parser = argparse.ArgumentParser(
            description='General manager to handle actions relative to'
                        ' building/testing libadalang'
        )
        self.subparsers = subparsers = args_parser.add_subparsers()

        args_parser.add_argument(
            '--build-dir', default='build',
            help=(
                'Directory to use for generated source code and binaries. By'
                ' default, use "build" in the current directory.'
            )
        )
        args_parser.add_argument(
            '--enable-static', action='store_true',
            help='Enabled the generation of static libraries'
        )
        args_parser.add_argument(
            '--disable-shared', action='store_true',
            help='Disable the generation (and testing) of shared libraries'
        )
        args_parser.add_argument(
            '--bindings', '-b', nargs='+', choices=('python', ),
            default=['python'],
            help='Bindings to generate (by default: only Python)'
        )
        args_parser.add_argument(
            '--verbose', '-v', action='store_true',
            help='Show verbose output'
        )

        ########
        # Help #
        ########

        self.help_parser = help_parser = subparsers.add_parser(
            'help', help=self.do_help.__doc__
        )
        help_parser.set_defaults(func=self.do_help)

        ############
        # Generate #
        ############

        self.generate_parser = generate_parser = subparsers.add_parser(
            'generate', help=self.do_generate.__doc__
        )
        generate_parser.add_argument(
            '--coverage', '-C', action='store_true',
            help='Compute code coverage for the code generator'
        )
        generate_parser.add_argument(
            '--pretty-print', '-p', action='store_true',
            help='Pretty-print generated source code'
        )
        generate_parser.set_defaults(func=self.do_generate)

        #########
        # Build #
        #########

        self.build_parser = build_parser = subparsers.add_parser(
            'build', help=self.do_build.__doc__
        )
        build_parser.add_argument(
            '--jobs', '-j', type=int, default=get_cpu_count(),
            help='Number of parallel jobs to spawn in parallel '
                 '(default: your number of cpu)'
        )
        build_parser.add_argument(
            '--build-mode', '-b', choices=list(self.BUILD_MODES),
            default='dev',
            help='Selects a preset for build options'
        )
        build_parser.add_argument(
            '--cargs', nargs='*', default=[],
            help='Options to pass as "-cargs" to GPRbuild'
        )
        build_parser.set_defaults(func=self.do_build)

        ########
        # Make #
        ########

        self.make_parser = make_parser = subparsers.add_parser(
            'make', help=self.do_make.__doc__
        )
        make_parser.add_argument(
            '--jobs', '-j', type=int, default=get_cpu_count(),
            help='Number of parallel jobs to spawn in parallel'
                 ' (default: your number of cpu)'
        )
        make_parser.add_argument(
            '--build-mode', '-b', choices=list(self.BUILD_MODES),
            default='dev',
            help='Selects a preset for build options'
        )
        make_parser.set_defaults(func=self.do_make)

        ###########
        # Install #
        ###########

        self.install_parser = install_parser = subparsers.add_parser(
            'install', help=self.do_install.__doc__
        )
        install_parser.add_argument(
            'install-dir',
            help='Installation directory.'
        )
        install_parser.set_defaults(func=self.do_install)

        ##########
        # Setenv #
        ##########

        self.setenv_parser = setenv_parser = subparsers.add_parser(
            'setenv', help=self.do_setenv.__doc__
        )
        setenv_parser.set_defaults(func=self.do_setenv)

        # The create_context method will create the context and set it here
        # only right before executing commands so that coverage computation
        # will apply to create_context.
        self.context = None

    def create_context(self, args):
        """
        Return a Langkit context (langkit.compile_context.CompileContext
        instance).

        This must be overriden by subclasses.
        """
        raise NotImplementedError()

    @property
    def lib_name(self):
        return self.context.ada_api_settings.lib_name

    def run(self, args=None):
        parsed_args = self.args_parser.parse_args()
        self.dirs.set_build_dir(parsed_args.build_dir)
        install_dir = getattr(parsed_args, 'install-dir', None)
        if install_dir:
            self.dirs.set_install_dir(install_dir)

        # Compute code coverage in the code generator if asked to
        if parsed_args.func == self.do_generate and parsed_args.coverage:
            try:
                import coverage
                del coverage
            except Exception as exc:
                import traceback
                print >> sys.stderr, 'Coverage not available:'
                traceback.print_exc(exc)
                sys.exit(1)

            cov = Coverage(self.dirs)
            cov.start()
        else:
            cov = None

        self.context = self.create_context(parsed_args)

        # Set the extensions dir on the compile context
        self.context.extensions_dir = self.dirs.lang_source_dir("extensions")

        parsed_args.func(parsed_args)

        if cov is not None:
            cov.stop()
            cov.generate_report()

    def do_generate(self, args):
        """Generate source code for libadalang."""
        printcol("Generating source for libadalang ...", Colors.HEADER)
        self.context.emit(file_root=self.dirs.build_dir())

        def gnatpp(project_file):
            try:
                subprocess.check_call([
                    'gnatpp',
                    '-P{}'.format(project_file),
                    '-XLIBRARY_TYPE=relocatable',
                    '-rnb',
                ], env=self.derived_env())
            except subprocess.CalledProcessError as exc:
                print >> sys.stderr, 'Pretty-printing failed: {}'.format(exc)
                sys.exit(1)

        if hasattr(args, 'pretty_print') and args.pretty_print:
            printcol("Pretty-printing sources for libadalang ...",
                     Colors.HEADER)
            gnatpp(self.dirs.build_dir('lib', 'gnat',
                                       '{}.gpr'.format(self.lib_name.lower())))
            gnatpp(self.dirs.build_dir('src', 'parse.gpr'))

        printcol("Generation complete!", Colors.OKGREEN)

    def do_build(self, args):
        """Build generated source code."""

        build_mode = args.build_mode if args.build_mode else 'dev'

        cargs = []
        # Depending on where this is invoked, the "cargs" option may not be set
        if hasattr(args, 'cargs'):
            cargs.extend(args.cargs)

        def gprbuild(project_file, is_dynamic):
            try:
                subprocess.check_call([
                    'gprbuild', '-m', '-p', '-j{}'.format(args.jobs),
                    '-P{}'.format(project_file),
                    '-XBUILD_MODE={}'.format(build_mode),
                    '-XLIBRARY_TYPE={}'.format(
                        'relocatable' if is_dynamic else 'static'
                    ),
                    '-XLIBLANG_SUPPORT_EXTERNALLY_BUILT=false',
                    '-X{}_EXTERNALLY_BUILT=false'.format(
                        self.lib_name.upper()
                    ),
                    '-cargs',
                ] + cargs, env=self.derived_env())
            except subprocess.CalledProcessError as exc:
                print >> sys.stderr, 'Build failed: {}'.format(exc)
                sys.exit(1)

        printcol("Building the generated source code ...", Colors.HEADER)
        lib_project = self.dirs.build_dir(
            'lib', 'gnat', '{}.gpr'.format(self.lib_name.lower())
        )
        if args.enable_static:
            gprbuild(lib_project, False)
        if not args.disable_shared:
            gprbuild(lib_project, True)

        printcol("Building the interactive test main ...", Colors.HEADER)
        if args.enable_static:
            gprbuild(self.dirs.build_dir('src', 'parse.gpr'), False)
        if not args.disable_shared:
            gprbuild(self.dirs.build_dir('src', 'parse.gpr'), True)

        # On Windows, shared libraries (DLL) are looked up in the PATH, just
        # like binaries (it's LD_LIBRARY_PATH on Unix). For this platform,
        # don't bother and just copy these DLL next to binaries.
        if os.name == 'nt':
            for dll in glob.glob(self.dirs.build_dir('lib', '*.dll')):
                shutil.copy(dll,
                            self.dirs.build_dir('bin', os.path.basename(dll)))

        printcol("Compilation complete!", Colors.OKGREEN)

    def do_make(self, args):
        """Generate and build in one command."""
        self.do_generate(args)
        self.do_build(args)

    def do_install(self, args):
        """Install programs and libraries."""
        del args

        for subdir in ('bin', 'include', 'lib', 'share', 'python'):
            install_dir = self.dirs.install_dir(subdir)
            if os.path.isdir(install_dir):
                shutil.rmtree(install_dir)
            assert not os.path.exists(install_dir)
            shutil.copytree(
                self.dirs.build_dir(subdir),
                self.dirs.install_dir(subdir)
            )

    def do_setenv(self, args):
        """
        Display Bourne shell commands that setup environment in order to make
        libadalang available.
        """
        def add_path(name, path):
            print('{name}={path}:${name}; export {name}'.format(
                name=name, path=pipes.quote(path)
            ))
        self.setup_environment(add_path)

    def do_help(self, args):
        """Print usage and exit."""
        self.args_parser.print_help()

    def setup_environment(self, add_path):
        add_path('PATH', self.dirs.build_dir('bin'))
        add_path('C_INCLUDE_PATH', self.dirs.build_dir('include'))
        add_path('LIBRARY_PATH', self.dirs.build_dir('lib'))
        add_path('LD_LIBRARY_PATH', self.dirs.build_dir('lib'))
        add_path('GPR_PROJECT_PATH', self.dirs.build_dir('lib', 'gnat'))
        add_path('PYTHONPATH', self.dirs.build_dir('python'))
        add_path('PYTHONPATH', self.dirs.lang_source_dir('python_src'))

    def derived_env(self):
        """
        Return a copy of the environment after an update using
        setup_environment.
        """
        env = dict(os.environ)

        def add_path(name, path):
            old = env.get(name, '')
            env[name] = ('{}{}{}'.format(path, os.path.pathsep, old)
                         if old else path)

        self.setup_environment(add_path)
        return env
