import libadalang

ctx = libadalang.AnalysisContext()
unit = ctx.get_from_file('foo.adb')

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

with_decl = unit.root.f_prelude[0]
assert with_decl.kind_name == 'WithDecl'
print 'WithDecl: is_limited = {}'.format(with_decl.f_is_limited)
print 'WithDecl: is_private = {}'.format(with_decl.f_is_private)


subp_body = unit.root.f_body.f_item
assert subp_body.kind_name == 'SubprogramBody'
print 'WithDecl: overriding = {}'.format(subp_body.f_overriding)

subp_name = subp_body.f_subp_spec.f_name
assert subp_name.kind_name == 'Identifier'
print 'Identifier: tok = {}'.format(subp_name.f_tok.text)

print "Done."
