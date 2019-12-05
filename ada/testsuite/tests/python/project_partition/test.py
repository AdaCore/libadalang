from __future__ import absolute_import, division, print_function

import libadalang as lal


for project in ('ap1.gpr', 'ap2.gpr'):
    print('Loading {}...'.format(project))
    try:
        up = lal.UnitProvider.for_project(project)
    except lal.InvalidProjectError as exc:
        print('   ... got a {} exception: {}'.format(type(exc).__name__, exc))
        continue
    else:
        print('   ... success!')

    ctx = lal.AnalysisContext(unit_provider=up)
    unit = ctx.get_from_provider('p2', lal.AnalysisUnitKind.unit_specification)
    ref = unit.root.find(lal.ObjectDecl).f_renaming_clause.f_renamed_object
    decl = ref.p_referenced_decl(imprecise_fallback=False)
    print('{} resolves to {}'.format(ref, decl))

print('Done.')
