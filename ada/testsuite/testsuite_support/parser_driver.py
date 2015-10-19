from testsuite_support.base_driver import (
    BaseDriver, catch_test_errors, SetupError,
)


class ParserDriver(BaseDriver):
    TIMEOUT = 300

    ACTIONS = ('pretty-print', 'indent', 'pretty-print-file',
               'pp-file-with-trivia')

    #
    # Driver entry points
    #

    @catch_test_errors
    def tear_up(self):
        super(ParserDriver, self).tear_up()

        # What should we do for this test?
        self.action = self.test_env.get('action', 'pretty-print')
        if self.action not in self.ACTIONS:
            raise SetupError('Invalid action: {}'.format(self.action))

        self.check_file('input')

    @catch_test_errors
    def run(self):
        input_file = self.working_dir('input')

        # Build the command line for the "parse" process we are going to run
        parse_argv = ['parse']

        try:
            charset = self.test_env['charset']
        except KeyError:
            pass
        else:
            parse_argv += ['-c', charset]

        if self.action == 'pretty-print-file':
            parse_argv += ['-f', input_file]
        elif self.action == 'pp-file-with-trivia':
            parse_argv += ['-P', '-f', input_file]
        else:
            rule_name = self.test_env.get('rule', None)
            if not rule_name:
                raise SetupError('Parsing rule is missing from test.yaml')
            parse_argv += ['-r', rule_name, self.read_file(input_file)]

        for lookup in self.get_lookups():
            parse_argv.append(
                '{}:{}'.format(lookup['line'], lookup['column'])
            )

        if self.action not in ('pretty-print', 'pretty-print-file',
                               'pp-file-with-trivia'):
            parse_argv.append('--silent')
        if self.action == 'indent':
            parse_argv.append('--indent')

        self.run_and_check(parse_argv, for_debug=True, memcheck=True)

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
