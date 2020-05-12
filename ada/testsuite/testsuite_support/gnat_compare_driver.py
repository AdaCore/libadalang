from testsuite_support.base_driver import BaseDriver


class GNATCompareDriver(BaseDriver):

    def run(self):
        project_file = self.test_env['project']
        comparisons = self.test_env.get('comparisons')

        argv = ['gnat_compare', '-P' + project_file]
        if comparisons:
            argv.append('-d{}'.format(comparisons))
        self.run_and_check(argv, memcheck=True)
