from __future__ import absolute_import, division, print_function

import libadalang


ctx = libadalang.AnalysisContext('iso-8859-1')
unit = ctx.get_from_file('foo.adb')

root = unit.root
assert root.kind_name == 'CompilationUnit'

try:
    tmp = root[3]
except IndexError as exc:
    pass
else:
    assert False, (
        "Out-of-bound (overflow) child access is not handled properly")

try:
    tmp = root[-10]
except IndexError as exc:
    pass
else:
    assert False, (
        "Out-of-bound (underflow) child access is not handled properly")

with_clause = unit.root.f_prelude[0]
assert with_clause.kind_name == 'WithClause'
print('WithClause: has_limited = {}'.format(with_clause.f_has_limited))
print('WithClause: has_private = {}'.format(with_clause.f_has_private))


subp_body = unit.root.f_body.f_item
assert subp_body.kind_name == 'SubpBody'
print('WithClause: overriding = {}'.format(subp_body.f_overriding))

subp_name = subp_body.f_subp_spec.f_subp_name
assert subp_name.kind_name == 'DefiningName'
print('DefiningName: text = {}'.format(subp_name.text))

print('Done.')
