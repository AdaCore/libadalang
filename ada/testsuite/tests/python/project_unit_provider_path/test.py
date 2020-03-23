"""
Test that libadalang.UnitProvider.for_project can resolve the designated
project from its path.
"""

from __future__ import absolute_import, division, print_function

import libadalang
from libadalang import _py2to3


def try_load(*args, **kwargs):
    try:
        libadalang.UnitProvider.for_project(*args, **kwargs)
    except libadalang.InvalidProjectError as exc:
        print('   exception {}: {}'.format(type(exc).__name__, exc))
    else:
        print('   success')


print('Load without project scope')
try_load('agg.gpr')

print('Load with ambiguous scope')
try_load('agg.gpr', project='arch')

print('Load with a project scope')
try_load('agg.gpr', project='arch32/arch.gpr')


print('Done.')
