from drivers.base_driver import BaseDriver


class GNATCompareDriver(BaseDriver):
    """
    Test driver to run the ``gnat_compare`` program and check LAL's nameres
    against GNAT's xrefs.

    The following list documents supported keys (in ``test.yaml``). All keys
    are assumed to be optional unless told otherwise.

    * ``project_file`` (mandatory): A filename for a project file, used to
      create a project unit provider.

    * ``comparisons``: Select what differences between GNAT's xrefs and
      Libadalang's to report.
    """

    def run(self):
        project_file = self.test_env['project_file']
        comparisons = self.test_env.get('comparisons')

        argv = ['gnat_compare', '-P' + project_file]
        if comparisons:
            argv.append('-d{}'.format(comparisons))
        self.run_and_check(argv, memcheck=True)
