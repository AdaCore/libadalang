from __future__ import absolute_import, division, print_function

import sys

import libadalang


def pflush(message):
    sys.stdout.write(message + '\n')
    sys.stdout.flush()

for project_file in ('invalid.gpr', 'does_not_exist.gpr'):
    pflush('Trying to load invalid project: {}'.format(project_file))
    try:
        ufp = libadalang.UnitProvider.for_project('invalid.gpr')
    except libadalang.InvalidProjectError as exc:
        pflush('   ... got an exception: {}'.format(exc))
    else:
        pflush('   ... got no exception. Unacceptable!')

ctx = libadalang.AnalysisContext(
    unit_provider=libadalang.UnitProvider.for_project('p.gpr')
)

for filename in ('\n', ' '):
    pflush('Trying to get unit: {}'.format(repr(filename)))
    try:
        unit = ctx.get_from_provider(
            filename, libadalang.AnalysisUnitKind.unit_body)
    except libadalang.InvalidUnitNameError as exc:
        pflush('   ... got an exception: {}'.format(exc))
    else:
        pflush('   ... got no exception. Unacceptable!')

print('Done.')
