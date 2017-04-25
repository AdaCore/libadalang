from __future__ import absolute_import, division, print_function

import libadalang

for src_dir in ('src1', 'src2'):
    print('For SRC_DIR={}:'.format(src_dir))
    ctx = libadalang.AnalysisContext(
        unit_provider=libadalang.UnitProvider.for_project(
            'p.gpr', {'SRC_DIR': src_dir}
        )
    )
    unit = ctx.get_from_provider('p2', 'specification')
    unit.populate_lexical_env()

    subtype_ind = unit.root.find(libadalang.SubtypeIndication)
    print('{} resolves to:'.format(subtype_ind))
    for entity in subtype_ind.f_name.p_entities:
        print ('  {}'.format(entity))

print('Done.')
