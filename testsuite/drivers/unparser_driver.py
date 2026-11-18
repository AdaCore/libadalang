from __future__ import annotations

import os.path
import re

from e3.testsuite.driver.classic import TestAbortWithError

from drivers.base_driver import BaseDriver


class UnparserDriver(BaseDriver):
    """
    Parse Ada source code with a given grammar rule, then unparsing it with the
    default unparsing configuration. Check both the Prettier document and the
    formatted source code against baselines.
    """

    input_filename = "input.ada"

    overriding_re = re.compile(
        "(?:(?P<prefix>[a-z][a-z-]*):)?"
        "(?P<location>[a-z][a-z0-9_-]*(?:/[a-z][a-z0-9_-]*)*)"
    )

    def in_ext_dir(self, *filenames: str) -> str:
        """
        ``os.path.join`` wrapper to return the absolute path to a file inside
        the extensions directory.
        """
        return os.path.join(self.env.root_dir, "..", "extensions", *filenames)

    @property
    def unparsing_config_filename(self) -> str:
        """
        Return the absolute path to the default unparsing configuration.
        """
        return self.in_ext_dir("default_unparsing_config.json")

    def parse_overriding_spec(self, spec: str) -> str:
        """
        Parse an overriding specifier and return the corresponding JSON
        filename.
        """
        m = self.overriding_re.fullmatch(spec)
        if m is None:
            raise TestAbortWithError(f"invalid overriding: {spec}")

        location = m.group("location")
        match m.group("prefix"):
            case None:
                return self.in_ext_dir(
                    "unparsing_overridings", f"{location}.json"
                )
            case "snippet":
                return os.path.join(
                    self.env.root_dir,
                    "..",
                    "unparsing_config_snippets",
                    location,
                    "snippet.json",
                )
            case _:
                raise TestAbortWithError(f"invalid prefix: {spec}")

    @property
    def unparsing_config_overridings(self) -> list[str]:
        """
        Return the absolute path to unparsing configuration overridings.
        """
        return [
            self.parse_overriding_spec(spec)
            for spec in self.test_env.get("overridings", [])
        ]

    def run(self) -> None:
        # Run the unparser on "input.ada" for the given grammar rule.
        # unparse it with the default unparsing configuration.
        argv = ["lal_unparse", self.input_filename]
        rule = self.test_env.get("rule")
        if rule:
            argv += ["-r", rule]

        # Even though we are using the default unparsing configuration, pass
        # the JSON file explicitly so that one does not need to rebuild
        # Libadalang in order to test a change in that configuration.
        argv += ["-c", self.unparsing_config_filename]

        # Pass the requested overriding files
        for filename in self.unparsing_config_overridings:
            argv += ["--overriding", filename]

        # Target 79 columns lines, 3 spaces for regular indentation, 2 spaces
        # for continuation indentation.
        argv += ["-w", "79", "-i", "3", "-I", "2"]

        # Enable unparsing auto-checks and check that all nodes have an
        # explicit configuration.
        argv += ["-A", "-C"]

        # Make runtime exceptions visible
        argv += ["-t", "expansion_errors"]

        self.run_and_check(argv, memcheck=True)
