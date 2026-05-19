import os.path
from typing import Dict, List

from drivers.base_driver import BaseDriver


class CheckAmbiguityDriver(BaseDriver):
    """
    Test driver to run the ``check_ambiguity`` program and check its output.

    The following list documents supported keys (in ``test.yaml``). All keys
    are assumed to be optional unless told otherwise.

    * ``input_sources``: A list of filenames for the source files to process.

    * ``charset``: The name of a string encoding to use when processing files.

    * ``project_file``: A filename for a project file, used to create a project
      unit provider.

    * ``project_path``: List of directory names (relative to the testsuite root
      directory) to add to the project files lookup path
      (``GPR_PROJECT_PATH``).

    * ``project_vars``: Name/value mapping for project files external
      variables.

    * ``auto_provider_dirs``: A list of directory names, used to create an auto
      unit provider.

    * ``subprojects``: List of subprojects in which to look for source files.
      If not passed, start from the root project only.

    * ``recursive``: If true, run on all units in the project tree, excluding
      externally built projects.

    * ``preprocessor_data_file``: Filename for the preprocessor data file.

    * ``preprocessor_path``: List of directories where preprocessor data files
      can be found.

    * ``runtime_name``: Name of the runtime to use when loading the project
      (``--RTS`` switch).
    """

    def run(self):

        args: List[str] = ["check_ambiguity"]
        """
        Arguments to pass to "check_ambiguity".
        """

        env: Dict[str, str] = {}
        """
        Additional environment variables for "check_ambiguity".
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

        # Some tests intentionally exercise cases with missing source files:
        # let "check_ambiguity" warn about them, but keep it going (-k).
        args.append("-k")

        project_file = self.test_env.get("project_file", None)
        if project_file:
            args.append(f"-P{project_file}")

        subprojects = self.test_env.get("subprojects", [])
        args.extend(f"--subproject={p}" for p in subprojects)

        for name, value in sorted(
            self.test_env.get("project_vars", {}).items()
        ):
            args.append(f"-X{name}={value}")

        auto_provider_dirs = self.test_env.get("auto_provider_dirs", None)
        if auto_provider_dirs:
            args.extend(
                "--auto-dir={}".format(d)
                for d in auto_provider_dirs
            )

        preprocessor_data_file = self.test_env.get("preprocessor_data_file",
                                                   None)
        preprocessor_path = self.test_env.get("preprocessor_path", [])
        if preprocessor_data_file:
            args.append(f"--preprocessor-data-file={preprocessor_data_file}")
            args.extend(f"--preprocessor-path={d}" for d in preprocessor_path)

        charset = self.test_env.get("charset")
        if charset:
            args.append(f"--charset={charset}")

        if self.test_env.get("recursive"):
            args.append("--recursive")

        runtime_name = self.test_env.get("runtime_name")
        if runtime_name:
            args.append(f"--RTS={runtime_name}")

        args += self.test_env.get("input_sources", [])

        cwd = self.test_env.get("cwd")
        if cwd:
            cwd = os.path.join(self.env.root_dir, cwd)

        self.run_and_check(argv=args, memcheck=True, env=env, cwd=cwd)
