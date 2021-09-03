import libadalang as lal


class App(lal.App):
    def process_unit(self, unit):
        for stmt in unit.root.findall(lal.Stmt):
            print(stmt, "is ghost:", stmt.p_is_ghost_code)

        for decl in unit.root.findall(lal.BasicDecl):
            print(decl, "is ghost:", decl.p_is_ghost_code)

        for pragma in unit.root.findall(lal.PragmaNode):
            print(pragma, "is ghost:", pragma.p_is_ghost_code)


if __name__ == "__main__":
    App.run()
