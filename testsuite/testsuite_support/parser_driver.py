import os.path
import pipes
import subprocess

from gnatpython.ex import Run, STDOUT
import gnatpython.fileutils as fileutils

from testsuite_support.base_driver import (
    BaseDriver, catch_test_errors, TestError,
)
from testsuite_support.valgrind import Valgrind


class ParserDriver(BaseDriver):
    TIMEOUT = 300

    #
    # Driver entry points.
    #

    @catch_test_errors
    def tear_up(self):
        # Testcases are expected to provide two files...
        for filename in ('input', 'expected'):
            self.check_file(os.path.join(self.test_dir, filename))

        self.create_test_workspace()

        self.valgrind = (
            Valgrind(self.working_dir)
            if self.global_env['options'].valgrind else None
        )

    @catch_test_errors
    def tear_down(self):
        pass

    @catch_test_errors
    def run(self):
        opts = self.global_env['options']
        rule_name = self.get_rule_name()
        lookups = self.get_lookups()
        input_text = self.read_file(self.input_file)

        parse_argv = ['parse', '-r', rule_name, '--input', input_text]
        for lookup in lookups:
            parse_argv.extend([
                '--lookup', '{}:{}'.format(lookup['line'], lookup['column'])
            ])

        if self.valgrind:
            parse_argv = self.valgrind.wrap_argv(parse_argv)

        # If we are running a debugger, we aren't even interested in the
        # result.
        if opts.debug:
            print('Running {} under a debugger...'.format(
                self.test_env['test_name']
            ))
            parse_argv = [opts.debugger, '--args'] + parse_argv
            print(' '.join(pipes.quote(arg) for arg in parse_argv))
            subprocess.check_call(parse_argv, cwd=self.working_dir)
            raise TestError('Test was running from a debugger: no result')
            return

        p = Run(parse_argv, cwd=self.working_dir,
                timeout=self.TIMEOUT,
                output=self.output_file,
                error=STDOUT)
        if p.status != 0:
            self.result.actual_output += self.read_file(self.output_file)
            self.result.set_status(
                'FAILED', 'parse returned nonzero: {}'.format(p.status)
            )

    def analyze(self):
        rewrite = self.global_env['options'].rewrite
        failures = []

        # Check for the test output itself.
        diff = fileutils.diff(self.expected_file, self.output_file)
        if diff:
            if rewrite:
                new_baseline = self.read_file(self.output_file)
                with open(self.original_expected_file, 'w') as f:
                    f.write(new_baseline)
            self.result.actual_output += diff
            failures.append('output is not as expected{}'.format(
                ' (baseline updated)' if rewrite else ''
            ))

        # Check memory issues if asked to.
        if self.valgrind:
            errors = self.valgrind.parse_report()
            if errors:
                self.result.actual_output += (
                    'Valgrind reported the following errors:\n{}'.format(
                        self.valgrind.format_report(errors)
                    )
                )
                failures.append('memory isuses detected')

        if failures:
            self.result.set_status('FAILED', ' | '.join(failures))
        else:
            self.result.set_status('PASSED')

    #
    # Convenience path builders
    #

    @property
    def input_file(self):
        return os.path.join(self.working_dir, 'input')

    @property
    def expected_file(self):
        return os.path.join(self.working_dir, 'expected')

    @property
    def original_expected_file(self):
        return os.path.join(self.test_dir, 'expected')

    #
    # Helpers
    #

    def get_rule_name(self):
        try:
            return self.test_env['rule']
        except KeyError:
            raise TestError(
                'The rule to used for parsing is missing from test.yaml'
            )

    def get_lookups(self):
        try:
            lookups = self.test_env['lookups']
        except KeyError:
            # Lookups are not required: we just test them if they are present.
            return []

        # Check that lookups are sane.
        for lookup in lookups:
            if (
                not isinstance(lookup, dict)
                or len(lookup) != 2
                or not isinstance(lookup.get('line'), int)
                or not isinstance(lookup.get('column'), int)
            ):
                raise TestError(
                    'Invalid lookup in test.yaml: {}'.format(lookup))

        return lookups
