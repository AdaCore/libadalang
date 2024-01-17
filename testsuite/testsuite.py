#! /usr/bin/env python

"""
Usage::

    testsuite.py [OPTIONS]

Run the Libadalang testsuite.
"""

import glob
import os
import shutil
import statistics
import subprocess
import sys
from typing import Any, Callable

from e3.collection.dag import DAG
from e3.testsuite import Testsuite, logger
from e3.testsuite.testcase_finder import ProbingError, YAMLTestFinder

from langkit.coverage import GNATcov

from drivers import (
    adaapi_driver, capi_driver, dda_driver, inline_pg_driver, java_driver,
    name_resolution_driver, navigation_driver, ocaml_driver, parser_driver,
    prep_driver, python_driver, unparser_driver,
)


class PerfTestFinder(YAMLTestFinder):
    """
    Testcase finder to use in perf mode.

    This finder automatically discard tests that do not have performance
    measuring instructions. This is preferable to creating these tests but
    skipping them (SKIP status) as most tests do not support performance
    measurements: less noise in the testsuite report.
    """

    def probe(self, testsuite, dirpath, dirnames, filenames):
        # Probe testcases as usual...
        result = super().probe(testsuite, dirpath, dirnames, filenames)

        # But reject testcases which do not contain performance measuring
        # instructions.
        if result is None or "perf" not in result.test_env:
            return None

        # Make sure that the driver supports performance measuring
        if not result.driver_cls.perf_supported:
            raise ProbingError(
                "this driver does not support performance measuring"
            )

        return result


class LALTestsuite(Testsuite):
    tests_subdir = 'tests'
    test_driver_map = {
        'ada-api': adaapi_driver.AdaAPIDriver,
        'c-api': capi_driver.CAPIDriver,
        'dda': dda_driver.DDADriver,
        'inline-playground': inline_pg_driver.InlinePlaygroundDriver,
        'java': java_driver.JavaDriver,
        'name-resolution': name_resolution_driver.NameResolutionDriver,
        'navigation': navigation_driver.NavigationDriver,
        'ocaml': ocaml_driver.OCamlDriver,
        'parser': parser_driver.ParserDriver,
        'prep': prep_driver.PrepDriver,
        'python': python_driver.PythonDriver,
        'unparser': unparser_driver.UnparserDriver,
    }

    # Even though we test Libadalang only in native configurations, this allows
    # us to know whether we are testing for a 32 or 64bit platform.
    enable_cross_support = True

    @property
    def test_finders(self):
        return [
            PerfTestFinder()
            if self.env.options.perf_mode
            else YAMLTestFinder()
        ]

    def add_options(self, parser):
        parser.add_argument(
            '--valgrind', action='store_true',
            help='Run tests within Valgrind to check memory issues.'
        )
        parser.add_argument(
            '--disable-shared', action='store_true',
            help='Disable tests involving shared libraries.'
        )
        parser.add_argument(
            '--disable-python', action='store_true',
            help='Disable tests involving the Python API.'
        )
        parser.add_argument(
            '--with-ocaml-bindings', default=None,
            help='If provided, must be a path from the current directory to'
                 ' the directory in which the OCaml bindings were generated.'
                 ' This enables tests involving the OCaml API (they are'
                 ' disabled by default).'
        )
        parser.add_argument(
            '--with-java-bindings', default=None,
            help='If provided, must be a path from the current directory to'
                 ' the directory where the Java bindings are located. If'
                 ' passed, this enables the tests for the Java API (disabled'
                 ' by default).'
        )
        parser.add_argument(
            '--with-python', default='python3',
            help='If provided, use as the Python interpreter in testcases.'
        )
        parser.add_argument(
            '--skip-internal-tests', action='store_true',
            help='Skip tests from the internal testsuite'
        )
        parser.add_argument(
            '--build-mode', default='dev',
            help='Build mode for Libadalang'
        )
        parser.add_argument(
            '--coverage', default=None,
            help='When provided, compute the code coverage of the testsuite'
                 ' and produce a report in the given directory. This requires'
                 ' GNATcoverage and a coverage build of Libadalang.'
        )
        parser.add_argument(
            '--gnatcov-instr-dir',
            help='Directory that contains instrumentation data files.'
        )
        parser.add_argument(
            '--restricted-env', action='store_true',
            help='Skip testcases that cannot run in a restricted environment'
                 ' (need for non-standard Python packages, assumptions on the'
                 ' source directory layout, etc.).'
        )

        parser.add_argument(
            '--perf-mode',
            help='Run the testsuite in performance mode: only run tests with'
                 ' instructions to measure performance. The argument is the'
                 ' directory in which to put profile data files.'
        )
        parser.add_argument(
            '--perf-no-profile',
            action='store_true',
            help='When running the testsuite in performance mode, only run'
                 ' the default perf measurements (no profile). This is useful'
                 ' to get feedback quickly during development.'
        )

        # Convenience options for developers
        parser.add_argument(
            '--rewrite', '-r', action='store_true',
            help='Rewrite test baselines according to current output.'
        )
        parser.add_argument(
            '--dda-compile', action='store_true',
            help='For DDA tests, compile sources to regenerate the JSON files.'
        )

        # Temporary discriminant while we support compilers that do not have
        # support for the Put_Image attribute.
        parser.add_argument(
            '--no-has-put-image',
            action='store_false',
            dest='has_put_image',
            help='Expect tests requiring support for the Put_Image attribute'
                 ' to fail.'
        )

    def set_up(self):
        super().set_up()
        opts = self.env.options

        # The perf mode is peculiar and incompatible with several other modes
        if opts.perf_mode:
            for enabled, option in [
                (opts.coverage, "--coverage"),
                (opts.valgrind, "--valgrind"),
            ]:
                if enabled:
                    logger.error(f"--perf-mode incompatible with {option}")
                    raise RuntimeError

        try:
            import pygments
        except ImportError:
            has_pygments = False
        else:
            del pygments
            has_pygments = True

        try:
            python_version = int(subprocess.check_output([
                opts.with_python,
                '-cimport sys; print(sys.version_info.major)'
            ]))
        except Exception as exc:
            logger.error('Python version auto-detection failed: {}'
                         .format(exc))
            raise

        # Compute a common evaluation environment for expression test execution
        # control.
        self.env.control_condition_env = {
            'restricted_env': opts.restricted_env,
            'os': self.env.build.os.name,
            'has_pygments': has_pygments,
            'python': python_version,
            'has_put_image': opts.has_put_image,
            'valgrind': opts.valgrind,
            'coverage': opts.coverage is not None,
        }

        ocaml_bindings = opts.with_ocaml_bindings
        self.env.ocaml_bindings = (os.path.abspath(ocaml_bindings)
                                   if ocaml_bindings else None)
        if self.env.ocaml_bindings:
            os.environ["LAL_OCAML_BINDINGS"] = self.env.ocaml_bindings

        java_bindings = opts.with_java_bindings
        self.env.java_bindings = (os.path.abspath(java_bindings)
                                  if java_bindings else None)

        # Ensure the testsuite starts with an empty directory to store
        # source trace files.
        traces_dir = os.path.join(self.working_dir, 'traces')
        if os.path.exists(traces_dir):
            shutil.rmtree(traces_dir)
        os.mkdir(traces_dir)
        self.env.traces_dir = traces_dir

        # If requested, enable the performance mode. In this case, make sure
        # that the directory in which to create profile data exists.
        if opts.perf_mode:
            perf_dir = os.path.abspath(opts.perf_mode)
            if not os.path.exists(perf_dir):
                os.mkdir(perf_dir)

            self.env.perf_mode = True
            self.env.perf_dir = perf_dir
            self.env.perf_no_profile = opts.perf_no_profile
        else:
            self.env.perf_mode = False

        self.env.rewrite_baselines = opts.rewrite

    def adjust_dag_dependencies(self, dag: DAG) -> None:
        # Fetch all tests which use the 'graal_c_api' mode of the Java driver,
        # for each test we need to get their main Java class and link it to the
        # generated native-image Java main.
        # Then the native-image compilation is done once for all.

        # If Java bindings are available, build a single test main for all
        # native-image tests in order to reduce the cost of native-image
        # compilation.
        if self.env.java_bindings:
            java_driver.JavaDriver.build_ni_main(
                self.working_dir,
                self.env.java_bindings,
                [
                    test_data.driver
                    for test_data in dag.vertex_data.values()
                    if (
                        isinstance(test_data.driver, java_driver.JavaDriver)
                        and test_data.driver.mode == "graal_c_api"
                    )
                ],
            )

    def tear_down(self):
        opts = self.main.args

        # If requested, produce a coverage report
        if opts.coverage:
            GNATcov().generate_report(
                title='Libadalang Coverage Report',
                instr_dir=opts.gnatcov_instr_dir,
                traces=glob.glob(os.path.join(self.env.traces_dir,
                                              '*', '*.srctrace')),
                output_dir=opts.coverage,
                working_dir=self.working_dir,
            )

        # If requested, display a short summary of performance metrics
        if self.env.perf_mode:
            self.perf_report()

        super().tear_down()

    def write_comment_file(self, f):
        f.write('Control condition env:')
        for k, v in sorted(self.env.control_condition_env.items()):
            f.write('\n  {}={}'.format(k, v))

    def perf_report(self) -> None:
        """
        Print a summary of performance metrics on the standard output.
        """
        print("Performance metrics:")

        def format_time(seconds: float) -> str:
            return "{:.2f}s".format(seconds)

        def format_memory(bytes_count: int) -> str:
            units = ["B", "KB", "MB", "GB"]
            unit = units.pop(0)
            while units and bytes_count > 1000:
                unit = units.pop(0)
                bytes_count /= 1000
            return "{:.2f}{}".format(bytes_count, unit)

        def compute_stats(numbers_str: str,
                          get_value: Callable[[str], Any],
                          format_value: Callable[[Any], str]) -> str:
            numbers = [get_value(n) for n in numbers_str.split()]
            return (
                f"{format_value(min(numbers))} .. {format_value(max(numbers))}"
                f" (median: {format_value(statistics.median(numbers))})"
            )

        # Print a summary of metrics for each testcase separately
        for test_name, entry in sorted(self.report_index.entries.items()):
            if "time" not in entry.info:
                continue
            print(f"* {test_name}:")
            print("  time: "
                  + compute_stats(entry.info["time"], float, format_time))
            print("  memory: "
                  + compute_stats(entry.info["memory"], int, format_memory))


if __name__ == '__main__':
    sys.exit(LALTestsuite().testsuite_main())
