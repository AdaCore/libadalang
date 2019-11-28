from __future__ import absolute_import, division, print_function

import libadalang as lal


def read_file(name):
    with open(name, 'rb') as f:
        return f.read()

ctx = lal.AnalysisContext()
pkg_ads = ctx.get_from_file("pkg.ads")
pkg_adb = ctx.get_from_file("pkg.adb")

call = pkg_adb.root.find(lal.CallStmt)
print("{} p_referenced_decl -> {}".format(
    call.f_call,
    call.f_call.p_referenced_decl()
))

print("now reparsing...")
pkg_adb = ctx.get_from_buffer(
    "pkg.adb",
    read_file("pkg-reparsed.adb"),
    reparse=True
)


call = pkg_adb.root.find(lal.CallStmt)
print("{} p_referenced_decl -> {}".format(
    call.f_call,
    call.f_call.p_referenced_decl()
))
