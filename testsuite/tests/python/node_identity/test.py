"""
Test that Python wrapper objects around C AST nodes are preserved across
different wrappings.
"""

import gc

import libadalang


ctx = libadalang.AnalysisContext()
unit = ctx.get_from_file('foo.adb')

# First check that user attributes on ASTNode are kept across wrappers.
node = unit.root
first_id = id(node)
node.foobar = 1

node = None
gc.collect()

# At this point, the first "node" wrapper is supposed to be free'd: create
# another one and check its attributes were preserved.
node = unit.root
last_id = id(node)
assert first_id != last_id, (
    'This testcase is meaningless if we get the same Python wrapper in the two'
    ' testing points (and we did).'
)

assert node.foobar == 1, (
    '"foobar" attribute was not preserved:'
    ' got {} instead of 1'.format(node.foobar)
)


# Now make sure we have a working equality operator
root1 = unit.root
root2 = unit.root
body = unit.root.f_body
assert root1 == root2
assert root1 != body

print 'Done.'
