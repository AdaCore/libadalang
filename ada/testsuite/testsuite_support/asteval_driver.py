from testsuite_support.base_driver import (
    BaseDriver, catch_test_errors, SetupError,
)


class ASTEvalDriver(BaseDriver):
    """
    Test driver used to leverage the ASTEval helper program.

    Interface:

    * in the "test.yaml" file:
      * add an "input_source" key that contains the name of the source file to
        parse;
      * add an "expressions" key that contains a list of strings; each string
        will be an ASTEval expression to evaluate;
    * put a "test.out" text file in the test directory.

    This driver will run the following command::

        "asteval <input_source> [<expression> ...]"

    The output of this will then be compared against the content of "test.out":
    tests pass iff both are identical.
    """

    TIMEOUT = 300

    #
    # Driver entry points
    #

    @catch_test_errors
    def tear_up(self):
        super(ASTEvalDriver, self).tear_up()

        # Make sure we have a source file to parse
        try:
            self.input_source = self.test_env['input_source']
        except KeyError:
            raise SetupError(
                'test.yaml: missing mandatory "input_source" key'
            )
        self.check_file(self.input_source)

        # Make sure we have ASTEval expressions to evaluate
        try:
            self.expressions = self.test_env['expressions']
        except KeyError:
            raise SetupError(
                'test.yaml: missing mandatory "expressions" key'
            )
        if (not isinstance(self.expressions, list) or
                not all(isinstance(expr, str) for expr in self.expressions)):
            raise SetupError(
                'test.yaml: "expressions" must be a list of strings'
            )

    @catch_test_errors
    def run(self):
        argv = ['asteval', self.input_source] + self.expressions
        self.run_and_check(argv, for_debug=True, memcheck=True)
