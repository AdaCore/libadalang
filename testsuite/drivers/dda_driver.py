import glob
import os
import os.path
from typing import Dict, List

from e3.fs import cp

from drivers.base_driver import BaseDriver


class DDADriver(BaseDriver):
    """
    Test driver to run the ``lal_dda`` program and check its output.

    The following ``test.yaml`` keys are supported. All are supposed to be
    optional unless specified mandatory.

    ``input_sources``

        A list of filenames for the source files to process.

    ``compiled_sources``

        Sources to compile with ``-gnatR4js`` when the testsuite
        ``--dda-compile`` option is active. If omitted, ``input_sources`` are
        compiled instead.

    ``project_file``

        A filename for a project file, used to create a project unit provider.

    ``project_path``

        List of directory names (relative to the testsuite root directory)
        to add to the project files lookup path (``GPR_PROJECT_PATH``).

    ``project_vars``

        Name/value mapping for project files external variables.
    """

    def run(self):
        # List of source files to process and unit provider
        input_sources = self.test_env.get("input_sources", [])

        # If requested, compile requested sources (compiled_sources if present,
        # otherwise input_sources) to regenerate the JSON files.  Do the
        # compilation in the working directory (for use in this testsuite run),
        # and copy the JSON files back to the test directory.
        compiled_sources = self.test_env.get("compiled_sources", input_sources)
        for f in compiled_sources:
            self.run_and_check(
                argv=["gcc", "-c", f, "-gnatR4js"], append_output=False,
            )

            # Even though sources may belong to subdirectories, GCC is run in
            # the working directory, so this is where the JSON files are
            # created.
            json_file = f"{os.path.basename(f)}.json"

            cp(self.working_dir(json_file), self.test_dir(json_file))

        # Command line arguments to run the "lal_dda" program
        args: List[str] = ["lal_dda"]

        # Additional environment variables for the subprocess
        env: Dict[str, str] = {}

        # Path for project files: make directories from the "project_path" key
        # prioritary and append existing paths from the environment.
        env["GPR_PROJECT_PATH"] = os.path.pathsep.join(
            [
                os.path.join(self.env.root_dir, p)
                for p in self.test_env.get("project_path", [])
            ]
            + os.environ.get("GPR_PROJECT_PATH", "").split(os.path.pathsep)
        )

        project_file = self.test_env.get("project_file", None)
        if project_file:
            args.append(f"-P{project_file}")

        for name, value in sorted(
            self.test_env.get("project_vars", {}).items()
        ):
            args.append(f"-X{name}={value}")

        # Add optional explicit list of sources to process
        args += input_sources

        # Load all representation info
        for f in sorted(glob.glob(self.test_dir("*.json"))):
            args += ["-i", f]

        self.run_and_check(argv=args, memcheck=True, env=env)
