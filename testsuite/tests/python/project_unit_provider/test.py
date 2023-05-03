import libadalang


vars = {b'SRC_DIR': b'src1'}


for args in [
    dict(project_file=None),
    dict(project_file=1),
    dict(project_file=b'p.gpr', scenario_vars={1: b'bar'}),
    dict(project_file=b'p.gpr', scenario_vars={b'bar': 1}),
    dict(project_file=u'p.gpr', scenario_vars=vars),
    dict(project_file=u'p.gpr',
         project=b'no_such_project', scenario_vars=vars),
    dict(project_file=u'p.gpr',
         project=u'no_such_project', scenario_vars=vars),
    dict(project_file=u'p.gpr', project=u'q', scenario_vars=vars),
]:
    print('Trying to build with {}'.format(repr(args)))
    try:
        libadalang.UnitProvider.for_project(**args)
    except (
        TypeError, libadalang.ProjectError, libadalang.UnsupportedViewError
    ) as exc:
        print('   ... got a {} exception: {}'.format(type(exc).__name__, exc))
    else:
        print('   ... success!')


for src_dir in ('src1', 'src2'):
    print('For SRC_DIR={}:'.format(src_dir))
    ctx = libadalang.AnalysisContext(
        unit_provider=libadalang.UnitProvider.for_project(
            project_file='p.gpr',
            scenario_vars={'SRC_DIR': src_dir}
        )
    )
    unit = ctx.get_from_provider(
        'p2', libadalang.AnalysisUnitKind.unit_specification)

    subtype_ind = unit.root.find(libadalang.SubtypeIndication)
    print('{} resolves to:'.format(subtype_ind))
    for entity in subtype_ind.f_name.p_matching_nodes:
        print('  {}'.format(entity))

print('Done.')
