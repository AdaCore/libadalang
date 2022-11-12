import libadalang as lal


class App(lal.App):
    def process_unit(self, unit):
        unit.root.dump()


if __name__ == '__main__':
    App.run()
