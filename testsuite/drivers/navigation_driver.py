from e3.testsuite.driver.classic import TestAbortWithError

from drivers.base_driver import BaseDriver


class NavigationDriver(BaseDriver):

    def run(self):
        try:
            input_sources = self.test_env['input_sources']
        except KeyError:
            raise TestAbortWithError(
                'Missing "input_sources" key in test.yaml'
            )
        if not isinstance(input_sources, list) or not all(
            isinstance(k, str) for k in input_sources
        ):
            raise TestAbortWithError(
                '"input_sources" must contain a list of strings'
            )

        try:
            kinds = self.test_env['kinds']
        except KeyError:
            raise TestAbortWithError('Missing "kinds" key in test.yaml')
        if not isinstance(kinds, list) or not all(
            isinstance(k, str) for k in kinds
        ):
            raise TestAbortWithError('"kinds" must contain a list of strings')

        # Some tests intentionally exercize cases with missing source files:
        # let "navigate" warn about them, but keep it going (-k).
        self.run_and_check(
            ['navigate', '-k', '-K', ','.join(kinds)]
            + input_sources,
            memcheck=True,
        )
