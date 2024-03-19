from __future__ import annotations
from drivers.base_driver import BaseDriver
import os.path


class GNATCompareDriver(BaseDriver):
    """
    Test driver to run the ``gnat_compare`` program and check LAL's nameres
    against GNAT's xrefs.

    The following list documents supported keys (in ``test.yaml``). All keys
    are assumed to be optional unless told otherwise.

    * ``project_file`` (mandatory): A filename for a project file, used to
      create a project unit provider.

    * ``project_path``: List of directory names (relative to the testsuite root
      directory) to add to the project files lookup path
      (``GPR_PROJECT_PATH``).

    * ``project_vars``: Name/value mapping for project files external
      variables.

    * ``preprocessor_data_file``: Filename for the preprocessor data file.

    * ``preprocessor_path``: List of directories where preprocessor data files
      can be found.

    * ``comparisons``: Select what differences between GNAT's xrefs and
      Libadalang's to report.
    """

    def run(self):
        env: dict[str, str] = {}
        """
        Additional environment variables for "nameres".
        """

        # Path for project files: make directories from the "project_path" key
        # prioritary and append existing paths from the environment.
        env["GPR_PROJECT_PATH"] = os.path.pathsep.join(
            [
                os.path.join(self.env.root_dir, p)
                for p in self.test_env.get("project_path", [])
            ]
            + os.environ.get("GPR_PROJECT_PATH", "").split(os.path.pathsep)
        )

        project_file = self.test_env['project_file']
        comparisons = self.test_env.get('comparisons')

        argv = ['gnat_compare', '-P' + project_file]
        if comparisons:
            argv.append('-d{}'.format(comparisons))

        for name, value in sorted(
            self.test_env.get("project_vars", {}).items()
        ):
            argv.append(f"-X{name}={value}")

        # Preprocessor handling
        preprocessor_data_file = self.test_env.get("preprocessor_data_file",
                                                   None)
        preprocessor_path = self.test_env.get("preprocessor_path", [])
        if preprocessor_data_file:
            argv.append(f"--preprocessor-data-file={preprocessor_data_file}")
            argv.extend(f"--preprocessor-path={d}" for d in preprocessor_path)

        self.run_and_check(argv, memcheck=True, env=env)
