import libadalang


ctx = libadalang.AnalysisContext()
unit = ctx.get_from_file('foo.adb')

root = unit.root
assert root.kind_name == 'CompilationUnit'
root.dump()
