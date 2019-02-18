from __future__ import absolute_import, division, print_function

from testsuite_support.base_driver import (
    BaseDriver, catch_test_errors, SetupError,
)


class ParserDriver(BaseDriver):

    ACTIONS = ('pretty-print', 'pretty-print-file',
               'pp-file-with-trivia', 'pp-file-with-lexical-envs')

    base_tree_dump_file = 'base-tree-dump.txt'
    unparsed_file = 'unparsed.txt'
    unparsed_tree_dump_file = 'unparsed-tree-dump.txt'

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

        # Pass a relative filename to "parse" so that its output does not
        # depend on the location of the working directory. This helps making
        # the test output stable across runs.
        self.input_file = self.test_env.get('input_file', 'input')

        self.check_file(self.input_file)

    @catch_test_errors
    def run(self):
        # Build the command line for the "parse" process we are going to run
        base_argv = ['parse']
        misc_argv = []

        base_argv += ['-f', self.input_file]

        charset = self.test_env.get('charset', None)
        if charset:
            base_argv += ['-c', charset]

        check_consistency = self.test_env.get('check-consistency', None)
        if check_consistency:
            base_argv += ['-C']

        rule_name = self.test_env.get('rule', None)
        if rule_name:
            base_argv += ['-r', rule_name]

        if self.action == 'pp-file-with-trivia':
            misc_argv += ['-P']
        elif self.action == 'pp-file-with-lexical-envs':
            misc_argv += ['-E']

        for lookup in self.get_lookups():
            misc_argv += [
                '-L',
                '{}:{}'.format(lookup['line'], lookup['column'])
            ]

        # Run a first time, to run the testcase according to "self.action"
        self.run_and_check(base_argv + misc_argv, for_debug=True,
                           memcheck=True)

        # If specifically asked not to test unparsing, stop now
        if not self.test_env.get('test-unparsing', True):
            return

        # Then run several other times to:
        #
        # 1. Get a sloc-less tree dump for the base input.
        # 2. Unparse  the base input.
        # 3. Get a sloc-less tree dump for the unparsed output.
        # 4. Check that both tree dumps are the same (i.e. that unparsing
        #    preserved the source).
        # For each step, save the result in a file to ease testsuite failure
        # investigation.
        outputs = {}
        for name, filename, argv in [
            ('base-tree-dump', self.base_tree_dump_file,
             base_argv + ['--hide-slocs']),
            ('unparsed', self.unparsed_file,
             base_argv + ['-s', '--unparse']),
            ('unparsed-tree-dump', self.unparsed_tree_dump_file,
             base_argv + ['-f', self.unparsed_file, '--hide-slocs']),
        ]:
            outputs[name] = self.run_and_check(argv, memcheck=True,
                                               append_output=False)
            with open(self.working_dir(filename), 'w') as f:
                f.write(outputs[name])

        # Do the comparison itself, report any difference
        diff = self.diff(self.working_dir(self.base_tree_dump_file),
                         self.working_dir(self.unparsed_tree_dump_file))
        if diff:
            self.result.actual_output += ('Difference between base tree dump'
                                          ' and unparsed tree dump:\n')
            self.result.actual_output += diff
            self.set_failure('unparsed diff')

    #
    # Helpers
    #

    def get_lookups(self):
        try:
            lookups = self.test_env['lookups']
        except KeyError:
            # Lookups are not required: we just test them if they are present
            return []

        # Check that lookups are sane
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
