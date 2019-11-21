from __future__ import absolute_import, division, print_function

from testsuite_support.base_driver import BaseDriver, catch_test_errors


class GNATCompareDriver(BaseDriver):

    @catch_test_errors
    def tear_up(self):
        super(GNATCompareDriver, self).tear_up()
        self.project_file = self.test_env['project']
        self.comparisons = self.test_env.get('comparisons')

    @catch_test_errors
    def run(self):
        argv = ['gnat_compare', '-P' + self.project_file]
        if self.comparisons:
            argv.append('-d{}'.format(self.comparisons))
        self.run_and_check(argv, for_debug=True, memcheck=True)
