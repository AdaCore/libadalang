from __future__ import absolute_import, division, print_function

import libadalang as lal


ctx = lal.AnalysisContext()

err = ctx.get_from_file("syntax_error.adb")
empty = ctx.get_from_file("empty.adb")
pkg = ctx.get_from_file("pkg.ads")

assert not empty.diagnostics and not pkg.diagnostics
assert err.diagnostics

foo = pkg.root.findall(lal.DefiningName)[1]
print("References to {}: {}".format(
    foo.p_basic_decl.p_fully_qualified_name,
    foo.p_find_all_references([pkg, err, empty])
))

print('Done')
