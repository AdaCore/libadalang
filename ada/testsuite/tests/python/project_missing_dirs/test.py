"""
Check that loading a project with missing directories does not emit warnings
about missing directories.
"""

from __future__ import absolute_import, division, print_function

import libadalang


libadalang.UnitProvider.for_project('p.gpr')
print('Done.')
