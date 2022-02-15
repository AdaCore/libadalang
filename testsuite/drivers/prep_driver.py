import os

from drivers.base_driver import BaseDriver


class PrepDriver(BaseDriver):
    """
    Test driver to exercize Libadalang's preprocessor.

    It runs the "lal_prep" test program one or several times with various
    arguments, determined by the "configs" key in "test.yaml."

    Each entry in the "configs" key must associate the name of an input file
    (entry key) with a list of command-lines to pass to "lal_prep" when running
    on the input file. For instance::

       configs:
          foo1.ada: ["", "-DX=Bar"]
          foo2.ada: ["-u -DX=Baz"]

    Will spawn the following commands::

       lal_prep foo1.ada
       lal_prep foo1.ada -DX=Bar
       lal_prep foo2.ada -u -DX=Baz

    If the "configs" key is missing, just run "lal_prep" on each file matching
    "*.ada" with no additional argument.
    """

    def output_append(self, content: str) -> None:
        if self.default_encoding == "binary":
            self.output += content.encode("ascii")
        else:
            self.output += content

    def run(self):
        configs = self.test_env.get("configs")
        if not configs:
            configs = {
                f: [""]
                for f in os.listdir(self.working_dir())
                if f.endswith(".ada")
            }

        for filename, options_list in sorted(configs.items()):
            self.output_append(f"== {filename} ==\n\n")
            for options in options_list:
                self.output_append(f"# {options or 'No option'}\n\n")
                self.run_and_check(
                    ["lal_prep", filename] + options.split(),
                    memcheck=True,
                )
                self.output_append("\n")
