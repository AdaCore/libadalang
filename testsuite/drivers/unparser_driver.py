from __future__ import annotations

import os.path

from drivers.base_driver import BaseDriver


class UnparserDriver(BaseDriver):
    """
    Parse Ada source code with a given grammar rule, then unparsing it with the
    default unparsing configuration. Check both the Prettier document and the
    formatted source code against baselines.
    """

    input_filename = "input.ada"

    @property
    def unparsing_config_filename(self) -> str:
        """
        Return the absolute path to the default unparsing configuration.
        """
        return os.path.join(
            self.env.root_dir,
            "..",
            "extensions",
            "default_unparsing_config.json",
        )

    def run(self) -> None:
        # Run the unparser on "input.ada" for the given grammar rule and
        # unparse it with the default unparsing configuration.
        argv = ["lal_unparse", "-w", "79", "-i", "3"]

        rule = self.test_env.get("rule")
        if rule:
            argv += ["-r", rule]

        argv += [self.unparsing_config_filename, self.input_filename]

        self.run_and_check(argv, memcheck=True)
