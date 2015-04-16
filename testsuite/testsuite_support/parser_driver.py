import os.path
import pipes
import subprocess

from gnatpython.ex import Run, STDOUT
import gnatpython.fileutils as fileutils

from testsuite_support.base_driver import (
    BaseDriver, catch_test_errors, SetupError,
)


class ParserDriver(BaseDriver):
    TIMEOUT = 300

    ACTIONS = ('pretty-print', 'indent')

    #
    # Driver entry points
    #

    @catch_test_errors
    def tear_up(self):
        super(ParserDriver, self).tear_up()

        self.check_file('input')

        try:
            self.rule_name = self.test_env['rule']
        except KeyError:
            raise SetupError(
                'The rule to used for parsing is missing from test.yaml'
            )

        # What should we do for this test?
        self.action = self.test_env.get('action', 'pretty-print')
        if self.action not in self.ACTIONS:
            raise SetupError('Invalid action: {}'.format(self.action))

    @catch_test_errors
    def run(self):
        opts = self.global_env['options']
        input_text = self.read_file(self.input_file)

        parse_argv = ['parse', '-r', self.rule_name, input_text]
        for lookup in self.get_lookups():
            parse_argv.append(
                '{}:{}'.format(lookup['line'], lookup['column'])
            )
        if self.action != 'pretty-print':
            parse_argv.append('--silent')
        if self.action == 'indent':
            parse_argv.append('--indent')

        self.run_and_check(parse_argv, for_debug=True, memcheck=True)

    #
    # Convenience path builders
    #

    @property
    def input_file(self):
        return os.path.join(self.working_dir, 'input')

    #
    # Helpers
    #

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
                raise SetupError(
                    'Invalid lookup in test.yaml: {}'.format(lookup))

        return lookups
