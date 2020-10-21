import libadalang as lal


def print_insts(entity):
    print('Instantiation chain:')
    insts = entity.p_generic_instantiations
    for g in insts:
        print('  * {}'.format(g))
    if not insts:
        print('  (none)')
    print('')


c = lal.AnalysisContext()
u = c.get_from_file('foo.ads')

v_name = u.root.findall(lal.Identifier)[-2]
v_decl = v_name.p_referenced_decl()
print('{} -> {}'.format(v_name, v_decl))
print_insts(v_decl)

foo_decl = u.root.find(lal.PackageDecl)
print(foo_decl)
print_insts(foo_decl)

print('Done.')
