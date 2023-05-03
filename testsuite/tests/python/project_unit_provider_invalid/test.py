import re
import sys

import libadalang


def pflush(message):
    sys.stdout.write(message + '\n')
    sys.stdout.flush()


def format_exc(exc):
    # For exceptions with no explicit message (e.g. Invalid_Project exceptions
    # from gnatcoll-projects.adb), hide the line number, which is out of our
    # control.
    return re.sub(r"([a-z_-]+\.adb):\d+", "\\1:XXX", str(exc))


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
    except libadalang.ProjectError as exc:
        pflush('   ... got an exception: ' + format_exc(exc))
    else:
        pflush('   ... got no exception')

# Then do something that is supposed to work
ctx = libadalang.AnalysisContext(
    unit_provider=libadalang.UnitProvider.for_project('p.gpr')
)

# And try to load units with various invalid names
for filename in (u'\xe9', u' '):
    pflush('Trying to get unit: {}'.format(ascii(filename)))
    try:
        unit = ctx.get_from_provider(
            filename, libadalang.AnalysisUnitKind.unit_body)
    except libadalang.InvalidUnitNameError as exc:
        pflush('   ... got an exception: {}'.format(exc))
    else:
        pflush('   ... got no exception')
        if unit.diagnostics:
            pflush('   ... but we got diagnostics:')
            for d in unit.diagnostics:
                print('{}: {}'.format(d.sloc_range, ascii(d.message)))

print('Done.')
