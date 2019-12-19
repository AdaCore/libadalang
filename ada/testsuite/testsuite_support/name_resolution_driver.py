from __future__ import absolute_import, division, print_function

from testsuite_support.base_driver import (BaseDriver, SetupError,
                                           catch_test_errors)


class NameResolutionDriver(BaseDriver):

    @catch_test_errors
    def tear_up(self):
        super(NameResolutionDriver, self).tear_up()

        if 'input_sources' not in self.test_env:
            raise SetupError('Missing "input_sources" key in test.yaml')
        self.input_sources = self.test_env['input_sources']

        self.charset = self.test_env.get('charset', None)
        self.project_file = self.test_env.get('project_file', None)

        self.auto_provider_dirs = self.test_env.get('auto_provider_dirs', None)

        self.imprecise_fallback = self.test_env.get('imprecise_fallback',
                                                    False)

    @catch_test_errors
    def run(self):
        args = list(self.input_sources)
        if self.charset:
            args.insert(0, '--charset={}'.format(self.charset))
        if self.project_file:
            args.insert(0, '-P{}'.format(self.project_file))
        if self.auto_provider_dirs:
            args = (['--auto-dir={}'.format(d)
                     for d in self.auto_provider_dirs] +
                    args)
        if self.imprecise_fallback:
            args.insert(0, '--imprecise-fallback')

        self.run_and_check(['nameres'] + args, for_debug=True, memcheck=True)
