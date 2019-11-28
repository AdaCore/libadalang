from __future__ import absolute_import, division, print_function

import libadalang
from libadalang import _py2to3


def unirepr(value):
    if isinstance(value, _py2to3.bytes_type):
        return _py2to3.bytes_repr(value)
    elif isinstance(value, _py2to3.text_type):
        return _py2to3.text_repr(value)
    elif isinstance(value, dict):
        return '{{{}}}'.format(', '.join(
            '{}: {}'.format(unirepr(key), unirepr(item))
            for key, item in value.items()
        ))
    elif isinstance(value, tuple):
        if len(value) == 1:
            return '({},)'.format(unirepr(value[0]))
        return '({})'.format(', '.join(unirepr(item) for item in value))
    else:
        return repr(value)


for args in [
    (None, ),
    (1, ),
    (b'p.gpr', {1: b'bar'}),
    (b'p.gpr', {b'bar': 1}),
    (u'p.gpr', {u'SRC_DIR': u'src1'}),
]:
    print('Trying to build with', unirepr(args))
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
    unit = ctx.get_from_provider(
        'p2', libadalang.AnalysisUnitKind.unit_specification)

    subtype_ind = unit.root.find(libadalang.SubtypeIndication)
    print('{} resolves to:'.format(subtype_ind))
    for entity in subtype_ind.f_name.p_matching_nodes:
        print ('  {}'.format(entity))

print('Done.')
