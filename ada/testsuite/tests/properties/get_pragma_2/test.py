import libadalang as lal


u = lal.AnalysisContext().get_from_file("test.adb")
assert not u.diagnostics

u_decl = u.root.find(lal.TypeDecl)
print("Declaration of U => {}".format(u_decl))
print("   get_pragma ('pack') => {}".format(u_decl.p_get_pragma('pack')))
print('')

u_decl = u.root.find(lal.DottedName).p_referenced_decl()
print("Declaration of U with rebindings of Pkg_I => {}".format(u_decl))
print("   get_pragma ('pack') => {}".format(u_decl.p_get_pragma('pack')))
print('')

print('Done')
