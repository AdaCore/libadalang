import os.path

import libadalang as lal


class App(lal.App):
    def process_unit(self, unit):
        print(f"Processing {os.path.basename(unit.filename)}")


App.run([])
