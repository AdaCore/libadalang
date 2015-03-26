"""
Test that Python wrapper objects around C AST nodes are preserved across
different wrappings.
"""

import libadalang


ctx = libadalang.AnalysisContext()
unit = ctx.create_from_file('foo.adb')


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


first_id = pre_process(unit)
last_id = post_process(unit)
assert first_id == last_id, (
    'Got different Python wrappers for the same C AST node')
print 'Done.'
