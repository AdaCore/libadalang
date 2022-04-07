import os
import os.path
import sys
from typing import Dict, List, Optional, Tuple, Union

from e3.testsuite.control import YAMLTestControlCreator
from e3.testsuite.driver.classic import (TestAbortWithError,
                                         TestAbortWithFailure, TestSkip)
from e3.testsuite.driver.diff import DiffTestDriver

from drivers.valgrind import Valgrind


class BaseDriver(DiffTestDriver):
    """
    Base class to provide common test driver helpers.
    """

    perf_supported = False
    """
    Whether this driver supports the perf mode.
    """

    @property
    def perf_mode(self) -> bool:
        """
        Return whether the performance mode is active.
        """
        return self.env.options.perf_mode

    @property
    def default_process_timeout(self):
        # In perf mode, disable process timeout as some profiling operations
        # can take a really long time to complete, and this is both okay and
        # hard to measure.
        return None if self.perf_mode else super().default_process_timeout

    @property
    def test_control_creator(self):
        return YAMLTestControlCreator(self.env.control_condition_env)

    def set_up(self):
        super().set_up()

        # If requested, skip internal testcases
        if (
            self.env.options.skip_internal_tests and
            self.test_env['test_name'].startswith('internal__tests')
        ):
            raise TestSkip('Skipping internal testcase')

        # If asked to run under Valgrind, prepare a Valgrind instance
        if self.env.options.valgrind:
            valgrind_supp = self.test_env.get('valgrind_suppressions', None)
            if valgrind_supp:
                valgrind_supp = self.test_dir(valgrind_supp)

            self.valgrind = Valgrind(self.env.root_dir, self.working_dir(),
                                     valgrind_supp)
        else:
            self.valgrind = None
        self.valgrind_errors = []

        self.process_counter = 0
        """
        Counter for subprocesses. Used to create unique filenames for a
        testcase.
        """

        # If False (the default), use the Python interpreter from the
        # testsuite's --with-python option. Otherwise, use the Python
        # interpreter that runs the testsuite.
        self.use_testsuite_python = bool(
            self.test_env.get('use_testsuite_python', False)
        )

        # If we are running this on Windows and this testcase has a
        # Windows-specific baseline, use it.
        windows_baseline_file = self.test_env.get("windows_baseline_file")
        if self.env.build.os.name == "windows" and windows_baseline_file:
            self.test_env.setdefault("baseline_file", windows_baseline_file)

    @property
    def baseline(self) -> Tuple[Optional[str], Union[str, bytes], bool]:
        # In perf mode, our purpose is to measure performance, not to check
        # results.
        return (None, "", False) if self.perf_mode else super().baseline

    @property
    def disable_shared(self):
        return self.env.options.disable_shared

    @property
    def disable_python(self):
        return self.env.options.disable_python

    @property
    def build_mode(self):
        return self.env.options.build_mode

    @property
    def python_interpreter(self):
        choices = {
            'default': self.env.options.with_python or 'python',
            'self': sys.executable,
        }

        key = self.test_env.get('python_interpreter', 'default')
        if not isinstance(key, str):
            raise TestAbortWithError(
                'Invalid "python_interpreter" key in test.yaml: it must'
                ' contain a string, got a {} instead'.format(
                    type(key).__name__))
        try:
            return choices[key]
        except KeyError:
            raise TestAbortWithError(
                'Invalid "python_interpreter" key in test.yaml: got {}, while'
                ' expecting one of: {}'.format(
                    repr(key), ', '.join(repr(k) for k in choices)))

    def check_file(self, filename):
        """
        Check file presence.

        If the file does not exist test is aborted.
        """
        if not os.path.isfile(self.test_dir(filename)):
            raise TestAbortWithError('Missing mandatory file: {}'
                                     .format(filename))

    def check_file_list(self, what, file_list, can_be_empty=True):
        """
        Raise a TestAbortWithError if `file_list` is not a list of existing
        files.

        Also raise an error if it is an empty list while `can_be_empty` is
        False.
        """
        # First check we have a list of strings
        if (not isinstance(file_list, list) or
                (not can_be_empty and len(file_list) == 0) or
                not all(isinstance(fn, str) for fn in file_list)):
            empty_msg = 'non-empty '
            raise TestAbortWithError(
                '{} must be a {}list of strings'.format(what, empty_msg))

        # Then check that these are existing files
        for filename in file_list:
            self.check_file(filename)

    #
    # Run helpers
    #

    def subp_env(self, env: Optional[Dict[str, str]] = None) -> Dict[str, str]:
        """
        Return the environment to use for a subprocess.

        :param env: Variables to define in the subprocess. They overwrite in
            the subprocess variables defined for the current process.
        """
        result = dict(os.environ)
        if env:
            result.update(env)

            self.result.log += "Env:\n"
            for name, value in sorted(env.items()):
                self.result.log += f"  {name}={value}\n"

        return result

    def run_and_check(self, argv, memcheck=False, append_output=True,
                      status_code=0, encoding=None, env=None):
        """
        Run a subprocess with `argv` and check it completes with status code 0.

        If `memcheck` is True then the program is run under Valgrind if asked
        to in the main testsuite driver. Any memory issue will be reported and
        turned into a testcase failure.

        Check that the subprocess's status code is `status_code`.

        In case of failure, the test output is appended to the actual output
        and a TestError is raised.
        """
        subp_env = self.subp_env(env)

        # If this testcase produced trace files, move them to the
        # testsuite-wide directory for later use.

        # In coverage mode, tell the subprogram to create its trace file in the
        # a testcase-specific subdir of the global traces directory. Create
        # that subdir if needed.
        if self.env.options.coverage:
            traces_dir = os.path.join(self.env.traces_dir,
                                      os.path.basename(self.working_dir()))
            if not os.path.exists(traces_dir):
                os.mkdir(traces_dir)
            self.process_counter += 1
            subp_env['LIBADALANG_TRACE_FILE'] = os.path.join(
                traces_dir, 'trace-{}.srctrace'.format(self.process_counter)
            )

        program = argv[0]

        # Run valgrind if asked to
        if memcheck and self.valgrind:
            argv = self.valgrind.wrap_argv(argv)

        # Do our own error checking (see the ``status_code`` argument)
        p = self.shell(argv, cwd=self.working_dir(), env=subp_env,
                       analyze_output=append_output,
                       catch_error=False,
                       encoding=encoding)

        if p.status != status_code:
            raise TestAbortWithFailure(
                '{} returned status code {} ({} expected)'
                .format(program, p.status, status_code))

        if memcheck and self.valgrind:
            self.valgrind_errors.extend(self.valgrind.parse_report())

        return p.out

    def run_for_perf(self,
                     argv: List[str],
                     env: Optional[Dict[str, str]] = None) -> None:
        """
        Run a subprocess and collect performance data from it.

        Time and memory metrics go to the ``TestResult.info`` table:

        * the "time" entry contains a space-separated list of floats for the
          time in seconds it took to run each instance of the subprocess;

        * the "memory" entry contains a space-separated list of integers for
          the approximative number of bytes each instance of the subprocess
          allocated.

        When created, files for time and memory profiles go to the "perf"
        working directory and the corresponding file names are stored in the
        "time-profile" and "memory-profile" entries in the ``TestResult.info``
        table.
        """
        perf_dir = self.env.perf_dir

        # Common arguments for "self.shell"
        subp_env = self.subp_env(env)
        cwd = self.working_dir()

        def run(*prefix: str) -> str:
            """
            Run the subprocess with the "prefix" additional arguments. Return
            the subprocess output.
            """
            return self.shell(
                args=list(prefix) + argv,
                cwd=cwd, env=subp_env,
                analyze_output=False
            ).out

        def data_file(suffix: str) -> str:
            """
            Return the name of the data file to use for this test, with the
            given suffix.
            """
            return f"{self.test_name}___{suffix}"

        # Run the subprocess in each of the requested performance measuring
        # mode.
        for mode, param in self.test_env["perf"].items():
            if mode == "default":
                # Run the subprocess several time under "time" to get its
                # execution time + maximum resident set size (memory occupied).
                time_list: List[str] = []
                memory_list: List[str] = []
                for i in range(param):
                    result = run("time", "-f", "%M %e")
                    memory, time = result.split()
                    time_list.append(time)
                    memory_list.append(str(int(memory) * 1024))
                self.result.info["time"] = " ".join(time_list)
                self.result.info["memory"] = " ".join(memory_list)

            elif mode == "profile-time":
                # Run the subprocess under "perf record" to get a profile (and
                # eventually a frame graph).
                f = data_file("perf.data")
                run(
                    "perf",
                    "record",
                    "--call-graph=dwarf",
                    "-F100",
                    "-o",
                    os.path.join(perf_dir, f),
                    "--"
                )
                self.result.info["time-profile"] = f

            elif mode == "profile-memory":
                # Run the subprocess under valgrind/massif to analyze where
                # memory is allocated.
                f = data_file("massif.data")
                fabs = os.path.join(perf_dir, f)
                run("valgrind", "--tool=massif", f"--massif-out-file={fabs}")
                self.result.info["memory-profile"] = f

            else:
                raise TestError(f"invalid perf mode: {mode}")

    @property
    def gpr_scenario_vars(self):
        """
        Return the project scenario variables to pass to GPRbuild.

        :rtype: list[str]
        """
        library_type = 'static' if self.disable_shared else 'relocatable'
        return ['-XLIBRARY_TYPE={}'.format(library_type),
                '-XXMLADA_BUILD={}'.format(library_type),
                '-XBUILD_MODE={}'.format(self.build_mode),

                # Make sure GPRbuild does not try to rebuild Libadalang, as
                # this will break other tests running in parallel.
                '-XLIBADALANG_EXTERNALLY_BUILT=true']

    def read_file(self, filename):
        """
        Return the content of `filename`, decoding it according to the
        default encoding.
        """
        if self.default_encoding == "binary":
            with open(filename, 'rb') as f:
                return f.read()
        else:
            with open(filename, 'r', encoding=self.default_encoding) as f:
                return f.read()

    def compute_valgrind_failures(self):
        if self.valgrind_errors:
            self.result.log += (
                'Valgrind reported the following errors:\n{}'.format(
                    self.valgrind.format_report(self.valgrind_errors)
                )
            )
            return ['memory issues detected']
        else:
            return []

    def compute_failures(self):
        return super().compute_failures() + self.compute_valgrind_failures()
