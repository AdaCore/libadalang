"""
Check that 'Annotate' configuration pragma are correctly taken into account
in the `get_annotations` property.
"""
import libadalang as lal


project = lal.GPRProject('prj.gpr')
context = project.create_context()

unit = context.get_from_file("pkg.ads")
td = unit.root.find(lal.TypeDecl)

print(td.p_get_annotations())
