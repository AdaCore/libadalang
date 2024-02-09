from __future__ import annotations

import os.path

from e3.testsuite.driver.classic import TestSkip

from drivers.base_driver import BaseDriver


class UnparserDriver(BaseDriver):
    """
    Parse Ada source code with a given grammar rule, then unparsing it with the
    default unparsing configuration. Check both the Prettier document and the
    formatted source code against baselines.
    """

    input_filename = "input.ada"
    document_baseline_filename = "doc-baseline.json"

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
        # TODO (eng/libadalang/prettier-ada#10) Prettier-ada is known to leak
        # memory. Until this is fixed, do not bother running related tests in
        # Valgrind mode.
        if self.env.options.valgrind:
            raise TestSkip("Prettier-ada is known to leak memory")

        # Run the unparser on "input.ada" for the given grammar rule and
        # unparse it with the default unparsing configuration, with a dump of
        # the Prettier document.
        argv = ["lal_unparse", "-d", "-w", "79", "-i", "3"]

        rule = self.test_env.get("rule")
        if rule:
            argv += ["-r", rule]

        argv += [self.unparsing_config_filename, self.input_filename]

        self.run_and_check(argv, memcheck=True)

    def compute_failures(self) -> list[str]:
        # Check the formatted source against "test.out"
        result = super().compute_failures()

        # Check the prettier document against "doc-baseline.json"

        def read_file(filename: str) -> str:
            with open(filename, "r", encoding=self.default_encoding) as f:
                return f.read()

        document_baseline_filename = self.test_dir(
            self.document_baseline_filename
        )
        document_baseline = read_file(document_baseline_filename)
        document_actual = read_file(self.working_dir("doc.json"))

        result.extend(self.compute_diff(
            baseline_file=document_baseline_filename,
            baseline=document_baseline,
            actual=document_actual,
            failure_message="Prettier document mismatch",
        ))

        return result
