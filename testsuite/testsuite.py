#! /usr/bin/env python

"""
Usage::

    testsuite.py [OPTIONS]

Run the Libadalang testsuite.
"""

import glob
import os
import shutil
import subprocess
import sys

from e3.testsuite import Testsuite, logger

from langkit.coverage import GNATcov

from drivers import (
    adaapi_driver, capi_driver, gnat_compare_driver,
    inline_pg_driver, name_resolution_driver, navigation_driver, ocaml_driver,
    parser_driver, python_driver
)


class LALTestsuite(Testsuite):
    tests_subdir = 'tests'
    test_driver_map = {
        'ada-api': adaapi_driver.AdaAPIDriver,
        'c-api': capi_driver.CAPIDriver,
        'navigation': navigation_driver.NavigationDriver,
        'ocaml': ocaml_driver.OCamlDriver,
        'parser': parser_driver.ParserDriver,
        'python': python_driver.PythonDriver,
        'gnat_compare': gnat_compare_driver.GNATCompareDriver,
        'name-resolution': name_resolution_driver.NameResolutionDriver,
        'inline-playground': inline_pg_driver.InlinePlaygroundDriver
    }

    # Even though we test Libadalang only in native configurations, this allows
    # us to know whether we are testing for a 32 or 64bit platform.
    enable_cross_support = True

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

        # Convenience options for developpers
        parser.add_argument(
            '--rewrite', '-r', action='store_true',
            help='Rewrite test baselines according to current output.'
        )

    def set_up(self):
        super().set_up()
        opts = self.env.options

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
            'valgrind': opts.valgrind,
        }

        ocaml_bindings = opts.with_ocaml_bindings
        self.env.ocaml_bindings = (os.path.abspath(ocaml_bindings)
                                   if ocaml_bindings else None)

        # Ensure the testsuite starts with an empty directory to store
        # source trace files.
        traces_dir = os.path.join(self.working_dir, 'traces')
        if os.path.exists(traces_dir):
            shutil.rmtree(traces_dir)
        os.mkdir(traces_dir)
        self.env.traces_dir = traces_dir

        self.env.rewrite_baselines = opts.rewrite

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

        super().tear_down()

    def write_comment_file(self, f):
        f.write('Control condition env:')
        for k, v in sorted(self.env.control_condition_env.items()):
            f.write('\n  {}={}'.format(k, v))


if __name__ == '__main__':
    sys.exit(LALTestsuite().testsuite_main())
