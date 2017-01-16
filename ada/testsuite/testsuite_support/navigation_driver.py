from testsuite_support.base_driver import (
    BaseDriver, catch_test_errors, SetupError
)


class NavigationDriver(BaseDriver):

    @catch_test_errors
    def tear_up(self):
        super(NavigationDriver, self).tear_up()

        try:
            self.input_sources = self.test_env['input_sources']
        except KeyError:
            raise SetupError('Missing "input_sources" key in test.yaml')
        if not isinstance(self.input_sources, list) or not all(
            isinstance(k, str) for k in self.input_sources
        ):
            raise SetupError('"input_sources" must contain a list of strings')

        try:
            self.kinds = self.test_env['kinds']
        except KeyError:
            raise SetupError('Missing "kinds" key in test.yaml')
        if not isinstance(self.kinds, list) or not all(
            isinstance(k, str) for k in self.kinds
        ):
            raise SetupError('"kinds" must contain a list of strings')

    @catch_test_errors
    def run(self):
        self.run_and_check(
            ['navigate', ','.join(self.kinds)] + self.input_sources,
            for_debug=True, memcheck=True,
        )
