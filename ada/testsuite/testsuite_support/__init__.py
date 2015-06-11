import gnatpython.testsuite

import testsuite_support.capi_driver
import testsuite_support.parser_driver
import testsuite_support.python_driver


class Testsuite(gnatpython.testsuite.Testsuite):
    TEST_SUBDIR = 'tests'
    DRIVERS = {
        'parser': testsuite_support.parser_driver.ParserDriver,
        'c-api': testsuite_support.capi_driver.CAPIDriver,
        'python': testsuite_support.python_driver.PythonDriver,
    }

    def add_options(self):
        self.main.add_option(
            '--valgrind', action='store_true',
            help='Run tests within Valgrind to check memory issues.'
        )
        self.main.add_option(
            '--disable-shared', action='store_true',
            help='Disable the testing of shared libraries'
        )
        self.main.add_option(
            '--with-python', default=None,
            help='If provided, use as the Python interpreter in testcases'
        )

        #
        # Convenience options for developpers
        #

        # Debugging
        self.main.add_option(
            '--debug', '-g', action='store_true',
            help='Run a test under a debugger'
        )
        self.main.add_option(
            '--debugger', '-G', default='gdb',
            help='Program to use as a debugger (default: gdb)'
        )

        # Tests update
        self.main.add_option(
            '--rewrite', '-r', action='store_true',
            help='Rewrite test baselines according to current output.'
        )

    def tear_up(self):
        super(Testsuite, self).tear_up()

        opts = self.global_env['options']
        assert not opts.valgrind or not opts.debug, (
            'Debugging while checking memory with Valgrind is not supported.')
