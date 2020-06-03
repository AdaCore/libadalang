from e3.testsuite.driver.classic import TestAbortWithError

from drivers.base_driver import BaseDriver


class NameResolutionDriver(BaseDriver):

    def run(self):
        if 'input_sources' not in self.test_env:
            raise TestAbortWithError(
                'Missing "input_sources" key in test.yaml'
            )
        input_sources = self.test_env['input_sources']

        charset = self.test_env.get('charset', None)
        project_file = self.test_env.get('project_file', None)
        auto_provider_dirs = self.test_env.get('auto_provider_dirs', None)
        imprecise_fallback = self.test_env.get('imprecise_fallback', False)

        args = list(input_sources)
        if charset:
            args.insert(0, '--charset={}'.format(charset))
        if project_file:
            args.insert(0, '-P{}'.format(project_file))
        if auto_provider_dirs:
            args = (['--auto-dir={}'.format(d) for d in auto_provider_dirs] +
                    args)
        if imprecise_fallback:
            args.insert(0, '--imprecise-fallback')

        self.run_and_check(['nameres'] + args, memcheck=True)
