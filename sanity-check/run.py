#! /usr/bin/env python

import glob
import os.path
import sys

import e3.sys
from e3.testsuite import Testsuite, logger
from e3.testsuite.driver.classic import TestAbortWithFailure
from e3.testsuite.driver.diff import DiffTestDriver


class AdaAPIDriver(DiffTestDriver):
    """Check that the Ada API works as expected."""

    def run(self):
        # Create a GPR file for the "main" executable
        gpr_file = self.working_dir("test.gpr")
        with open(gpr_file, "w") as f:
            f.write("""\
with "libadalang";

project Test is
   for Main use ("main.adb");
   for Object_Dir use "obj";
end Test;
""")

        # Build that executable and run it
        self.shell(["gprbuild", "-P", "test.gpr"], analyze_output=False)
        self.shell([self.working_dir("obj", "main")])


class PythonAPIDriver(DiffTestDriver):
    """Check that the Python API works as expected."""

    def run(self):
        # Look for the Libadalang wheel to install. We expect exactly one.
        wheels_pattern = os.path.join(
            self.env.libadalang_prefix,
            "share",
            "libadalang",
            "python",
            "Libadalang*.whl",
        )
        wheels = glob.glob(wheels_pattern)
        if len(wheels) != 1:
            wheel_str = (
                "\n".join(f"* {filneame}" for filename in wheels)
                or "  <none>"
            )
            raise TestAbortWithFailure(
                "Exactly one Libadalang Python wheel expected. Found:\n"
                + wheel_str
                + f"\n(pattern: {wheels_pattern}"
            )
        wheel = wheels[0]

        # Create a venv using that interpreter and install Libadalang's Python
        # bindings in it.
        venv_dir = self.working_dir("venv")
        self.shell(
            [self.env.python_interpreter, "-m", "venv", venv_dir],
            analyze_output=False,
        )
        venv_interp = e3.sys.interpreter(venv_dir)
        self.shell(
            [venv_interp, "-m", "pip", "install", wheel], analyze_output=False
        )

        # Run the test script
        self.shell([venv_interp, "main.py"])


class SanityCheckTestsuite(Testsuite):
    """Testsuite to sanity check the customer-ready Libadalang package.

    The purpose of this testsuite is to make sure that the customer-ready
    Libadalang package, once installed, works as expected, i.e. whether it is
    possible to use public Ada/Python APIs.
    """

    test_driver_map = {
        "ada_api": AdaAPIDriver,
        "python_api": PythonAPIDriver,
    }

    # Even though we test Libadalang only in native configurations, this allows
    # us to know whether we are testing for a 32 or 64bit platform.
    enable_cross_support = True

    def add_options(self, parser):
        parser.add_argument(
            "--libadalang-prefix",
            help="Mandatory: installation prefix for Libadalang.",
        )
        parser.add_argument(
            "--with-python",
            default="python3",
            help="If provided, Python interpreter to load Libadalang in"
            " testcases.",
        )

        # Convenience options for developers
        parser.add_argument(
            '--rewrite', '-r', action='store_true',
            help='Rewrite test baselines according to current output.'
        )

    def set_up(self):
        self.env.rewrite_baselines = self.main.args.rewrite
        self.env.python_interpreter = self.main.args.with_python

        # Ensure we have access to the installation prefix for Libadalang. Turn
        # it into an absolute path so that tests can use it wherever they run
        # on the filesystem.
        if not self.main.args.libadalang_prefix:
            logger.error("Missing mandatory --libadalang-prefix argument")
            sys.exit(1)
        self.env.libadalang_prefix = os.path.abspath(
            self.main.args.libadalang_prefix
        )


if __name__ == '__main__':
    sys.exit(SanityCheckTestsuite().testsuite_main())
