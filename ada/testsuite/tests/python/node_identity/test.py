"""
Test that Python wrapper objects around C AST nodes are preserved across
different wrappings.
"""

from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

import libadalang


ctx = libadalang.AnalysisContext('iso-8859-1')
unit = ctx.get_from_file('foo.adb')


def pre_process(unit):
    root = unit.root
    root.foobar = 1
    return id(root)


def post_process(unit):
    root = unit.root
    assert root.foobar == 1, (
        'root.foobar = {} whereas 1 was expected instead'.format(
            unit.foobar
        )
    )
    return id(root)

# Now make sure we have a working equality operator
root1 = unit.root
root2 = unit.root
body = unit.root.f_body
assert root1 == root2
assert root1 != body

first_id = pre_process(unit)
last_id = post_process(unit)
assert first_id == last_id, (
    'Got different Python wrappers for the same C AST node')
print('Done.')
