from __future__ import absolute_import, division, print_function

import libadalang


for args in [
    (None, ),
    (1, ),
    ('p.gpr', {1: 'bar'}),
    ('p.gpr', {'bar': 1}),
    (u'p.gpr', {u'SRC_DIR': u'src1'}),
]:
    print('Trying to build with', args)
    try:
        libadalang.UnitProvider.for_project(*args)
    except TypeError as exc:
        print('   ... got a TypeError exception:', exc)
    else:
        print('   ... success!')


for src_dir in ('src1', 'src2'):
    print('For SRC_DIR={}:'.format(src_dir))
    ctx = libadalang.AnalysisContext(
        unit_provider=libadalang.UnitProvider.for_project(
            'p.gpr', {'SRC_DIR': src_dir}
        )
    )
    unit = ctx.get_from_provider('p2', 'specification')

    subtype_ind = unit.root.find(libadalang.SubtypeIndication)
    print('{} resolves to:'.format(subtype_ind))
    for entity in subtype_ind.f_name.p_matching_nodes:
        print ('  {}'.format(entity))

print('Done.')
