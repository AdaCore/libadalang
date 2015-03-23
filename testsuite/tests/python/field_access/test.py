import libadalang

ctx = libadalang.AnalysisContext()
unit = ctx.create_from_file('foo.adb')

root = unit.root
assert root.kind_name == 'CompilationUnit'

try:
    tmp = root[2]
except IndexError as exc:
    pass
else:
    assert False, (
        "Out-of-bound (overflow) child access is not handled properly")

try:
    tmp = root[-1]
except IndexError as exc:
    pass
else:
    assert False, (
        "Out-of-bound (underflow) child access is not handled properly")

with_decl = unit.root.prelude[0]
assert with_decl.kind_name == 'WithDecl'
print 'WithDecl: is_limited = {}'.format(with_decl.is_limited)
print 'WithDecl: is_private = {}'.format(with_decl.is_private)


subp_body = unit.root.body.item
assert subp_body.kind_name == 'SubprogramBody'
print 'WithDecl: overriding = {}'.format(subp_body.overriding)
print "Done."
