import libadalang as lal


ctx = lal.AnalysisContext()
u = ctx.get_from_file("test.ads")
td = u.root.find(lal.TypeDecl)
base_name = td.f_type_def.f_subtype_indication.f_name
vec = base_name.p_referenced_decl()
noop = vec.p_next_part.p_previous_part()
assert (noop is not None)
print("Done.")
