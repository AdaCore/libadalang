from e3.testsuite.driver.classic import TestAbortWithError

from drivers.base_driver import BaseDriver


class ParserDriver(BaseDriver):

    ACTIONS = ('pretty-print', 'pretty-print-file',
               'pp-file-with-trivia', 'pp-file-with-lexical-envs')

    base_tree_dump_file = 'base-tree-dump.txt'
    unparsed_file = 'unparsed.txt'
    unparsed_tree_dump_file = 'unparsed-tree-dump.txt'

    @property
    def test_unparsing(self):
        """
        Whether to test unparsing on this testcase.

        :rtype: bool
        """
        return self.test_env.get('test-unparsing', True)

    def log_file(self, name):
        """
        Return the path to the log file for the ``name`` testing step.

        :type name: str
        :rtype: str
        """
        return self.working_dir(name + '.txt')

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
                raise TestAbortWithError(
                    'Invalid lookup in test.yaml: {}'.format(lookup))

        return lookups

    def run(self):
        # What should we do for this test?
        action = self.test_env.get('action', 'pretty-print')
        if action not in self.ACTIONS:
            raise TestAbortWithError('Invalid action: {}'.format(action))

        # Pass a relative filename to "parse" so that its output does not
        # depend on the location of the working directory. This helps making
        # the test output stable across runs.
        input_file = self.test_env.get('input_file', 'input')
        self.check_file(input_file)

        # Build the command line for the "parse" process we are going to run
        base_argv = ['lal_parse']
        misc_argv = []

        file_args = ['-f', input_file]

        charset = self.test_env.get('charset', None)
        if charset:
            base_argv += ['-c', charset]

        check_consistency = self.test_env.get('check-consistency', None)
        if check_consistency:
            base_argv += ['-C']

        rule_name = self.test_env.get('rule', None)
        if rule_name:
            base_argv += ['-r', rule_name]

        if action == 'pp-file-with-trivia':
            misc_argv += ['-P']
        elif action == 'pp-file-with-lexical-envs':
            misc_argv += ['-E']

        for lookup in self.get_lookups():
            misc_argv += [
                '-L',
                '{}:{}'.format(lookup['line'], lookup['column'])
            ]

        self.outputs = {}

        def run(name, argv, append_output=False, encoding=None):
            encoding = encoding or self.default_encoding
            out = self.run_and_check(
                argv,
                memcheck=True,
                append_output=append_output,
                encoding=encoding
            )
            self.outputs[name] = out
            if encoding == "binary":
                with open(self.log_file(name), 'wb') as f:
                    f.write(out)
            else:
                with open(self.log_file(name), 'w', encoding=encoding,
                          newline='') as f:
                    f.write(out)

        # Run a first time, to run the testcase according to "action". Encoding
        # should not matter here, since parse's default output only uses ASCII.
        run('name', base_argv + file_args + misc_argv,
            append_output=True)

        # If specifically asked not to test unparsing, stop now
        if not self.test_unparsing:
            return

        # Then run several other times to:
        #
        # 1. Get a sloc-less tree dump for the base input.
        # 2. Unparse the base input.
        # 3. Get a sloc-less tree dump for the unparsed output.
        # 4. Check that both tree dumps are the same (i.e. that unparsing
        #    preserved the source).
        #
        # For each step, save the result in a file to ease testsuite failure
        # investigation. Note that except for the unparsing itself, parse's
        # output uses only ASCII. Read unparsing as binary: the only use for
        # its output is to drive the second parsing.
        for name, argv, encoding in [
            (
                'base-tree-dump',
                base_argv + file_args + ['--hide-slocs'],
                None
            ),
            (
                'unparsed',
                base_argv + file_args + ['-s', '--unparse'],
                'binary'
            ),
            (
                'unparsed-tree-dump',
                base_argv + ['-f', self.unparsed_file, '--hide-slocs'],
                None
            ),
        ]:
            run(name, argv, encoding=encoding)

    def compute_failures(self):
        failures = super(ParserDriver, self).compute_failures()
        if not self.test_unparsing:
            return failures

        # Compare the output of the second tree dump with the output of the
        # third one.
        failures.extend(self.compute_diff(
            None,
            self.read_file(self.log_file('base-tree-dump')),
            self.outputs['unparsed-tree-dump'],
            failure_message='second & third tree dump mismatch'
        ))

        return failures
