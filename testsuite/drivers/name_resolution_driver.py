from typing import List

from e3.testsuite.driver.classic import TestAbortWithError

from drivers.base_driver import BaseDriver


class NameResolutionDriver(BaseDriver):
    """
    Test driver to run the ``nameres`` program and check its output.

    The following list documents supported keys (in ``test.yaml``). All keys
    are assumed to be optional unless told otherwise.

    ``input_sources`` (mandatory)

        A list of filenames for the source files to process.

    ``charset``

        The name of a string encoding to use when processing files.

    ``project_file``

        A filename for a project file, used to create a project unit provider.

    ``auto_provider_dirs``

        A list of directory names, used to create an auto unit provider.

    ``imprecise_fallback``

        Boolean (false by default) to trigger implicit fallbacks during name
        resolution.

    ``preprocesor_data_file``

        Filename for the preprocessor data file.

    ``preprocessor_path``

        List of directories where preprocessor data files can be found.
    """

    def run(self):

        args: List[str] = []
        """
        Optional arguments to pass to "nameres".
        """

        # List of source files to process and unit provider
        if "input_sources" not in self.test_env:
            raise TestAbortWithError(
                "Missing 'input_sources' key in test.yaml"
            )
        input_sources = self.test_env["input_sources"]

        project_file = self.test_env.get("project_file", None)
        if project_file:
            args.append(f"-P{project_file}")

        auto_provider_dirs = self.test_env.get("auto_provider_dirs", None)
        if auto_provider_dirs:
            args.extend(
                "--auto-dir={}".format(d)
                for d in auto_provider_dirs
            )

        # Preprocessor handling
        preprocessor_data_file = self.test_env.get("preprocessor_data_file",
                                                   None)
        preprocessor_path = self.test_env.get("preprocessor_path", [])
        if preprocessor_data_file:
            args.append(f"--preprocessor-data-file={preprocessor_data_file}")
            args.extend(f"--preprocessor-path={d}" for d in preprocessor_path)

        # Various options
        charset = self.test_env.get("charset")
        if charset:
            args.append(f"--charset={charset}")

        if self.test_env.get("imprecise_fallback"):
            args.append("--imprecise-fallback")

        self.run_and_check(["nameres"] + args + input_sources, memcheck=True)
