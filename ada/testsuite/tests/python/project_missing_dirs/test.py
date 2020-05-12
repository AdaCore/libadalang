"""
Check that loading a project with missing directories does not emit warnings
about missing directories.
"""

import libadalang


libadalang.UnitProvider.for_project('p.gpr')
print('Done.')
