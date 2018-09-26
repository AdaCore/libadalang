from __future__ import absolute_import, division, print_function

import sys

import libadalang


def pflush(message):
    sys.stdout.write(message + '\n')
    sys.stdout.flush()

# Make several invalid attempts at loading projects
for label, project_file, kwargs in [
    ('invalid project', 'invalid.gpr', {}),
    ('not existing', 'does_not_exist.gpr', {}),

    # For now, loading a project with an unknown target seems not to be an
    # error in GNATCOLL.Projects... fine, at least let's check that this leads
    # to no crash.
    ('project with unknown target', 'p.gpr', {'target': 'nosuchtarget',
                                              'runtime': 'nosuchrts'}),
]:
    pflush('Trying to load {}:'.format(label))
    try:
        ufp = libadalang.UnitProvider.for_project(project_file, **kwargs)
    except libadalang.InvalidProjectError as exc:
        pflush('   ... got an exception: {}'.format(exc))
    else:
        pflush('   ... got no exception')

# Then do something that is supposed to work
ctx = libadalang.AnalysisContext(
    unit_provider=libadalang.UnitProvider.for_project('p.gpr')
)

# And try to load units with various invalid names
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
